#!/usr/bin/env ruby

require 'colorize'

MYVERSION = "0.1"
puts "Trombone version #{MYVERSION}".underline

begin
  require 'C2S'
  require 'scriptprelude'
rescue LoadError
  raise "Missing c2s library; include its path in $RUBYLIB."
end

require_relative 'serencoding'

$cleanup = true
$graph = false

def usage()
    puts "usage: trombone.rb <impl>.bpl /cycle:_ /rounds:_ /delayBound:_"
end

def parse_args()
  bplsources, rest = ARGV.partition{|f| File.extname(f) == ".bpl"}
  clangsources, rest = rest.partition{|f| File.extname(f) =~ /[.](c|cc|cpp)/}
  cycle, rest = rest.partition{|a| a =~ /\/cycle:[0-9]+/}
  deferred, rest = rest.partition{|a| a =~ /\/deferredUpdate/}
  rounds, rest = rest.partition{|a| a =~ /\/rounds:[0-9]+/}
  delays, rest = rest.partition{|a| a =~ /\/delayBound:[0-9]+/}
  m2s, rest = rest.partition{|a| a =~ /\/multitosingle/}
  keep, rest = rest.partition{|a| a =~ /\/keepFiles/}
  graph, rest = rest.partition{|a| a =~ /\/graph(Of)?Trace/}
  recbound, _ = rest.partition{|a| a=~ /\/recursionBound:[0-9]+/}
  traceinfo, rest = rest.partition{|a| a =~ /\/traceInfo/}

  boogieflags, rest = rest.partition{|a| a =~ /\/B[^ ]*/}
  clangflags, rest = rest.partition{|a| a =~ /\/C[^ ]*/}
  c2sflags, rest = rest.partition{|a| a =~ /\/S[^ ]*/}
  
  clangflags = clangflags.map{|a| a[2..a.length]}
  
  if bplsources.empty? and clangsources.empty? then
  	err "Please specify at least one Clang (.c, .cc, .cpp) or Boogie (.bpl) source file."
  	exit -1
  end
  
  if not bplsources.empty? and not clangsources.empty? then
    err "Don't know how to handle both Clang and Boogie sources at the same time."
    exit -1
  end
  
  sources = bplsources + clangsources
  
  sources.each do |f|
    if not File.exists?(f) then
      err "Cannot find file `#{f}'."
      exit -1
    end
  end
  
  # ToDo: find a cleaner way to locate this file
  # sources << bpl_source("#{File.dirname(__FILE__)}/../aux/ser-encoding-mje.bpl")
  
  if cycle.empty? then
    puts "Using default /cycle:2."
    $cycle = 2
  else
    $cycle = cycle.first.sub(/\/cycle:([0-9]+)/, '\1').to_i
  end
  
  if deferred.empty? then
    puts "Using default DIRECT update semantics."
    $deferred = false
  else
    puts "Using DEFERRED update semantics."
    $deferred = true
  end
  
  if delays.empty? then
    puts "Using default /delayBound:0."
    delays = 0
  else 
    delays = delays.first.sub(/\/delayBound:([0-9]+)/, '\1').to_i
  end

  if rounds.empty? then
  	puts "Using default /rounds:#{delays+1}, for /delayBound:#{delays}."
    rounds = delays + 1
  else
    rounds = rounds.first.sub(/\/rounds:([0-9]+)/, '\1').to_i
  end
  
  # just read the recursion bound to know how far loops will be unrolled
  if recbound.empty? then
    $recbound = nil
  else
    $recbound = recbound.first.sub(/\/recursionBound:([0-9]+)/, '\1').to_i
  end
  
  $traceinfo = !traceinfo.empty?

  m2s = !m2s.empty?
  $cleanup = keep.empty?
  $graph = !graph.empty?
  
  return clangsources, clangflags, bplsources, rounds, delays, rest
end 

def cleanup( files )
  File.delete( *files ) if $cleanup
end

def resolve_tm_operations( src )  
  puts "Resolving the TM operations..".underline
  text = File.read(src)
  pat = /[_A-Za-z0-9]*(TMInit|TXBegin|TXRead|TXWrite|TXCommit|TXAbort)[_A-Za-z0-9]*/
  File.open(src, 'w') do |f|
    f.puts text.gsub(pat,'\1')
  end  
  puts "done."
  puts " #{"-"*78} "  
end
  
# Inject the cycle-size specific axioms
def inject_tm_harness( src, cycle = 2, recbound = nil, deferred = false, traceinfo = false, ptrs = false )
  File.open(src, 'a') do |f|
    f.puts "#{harnessXXX(cycle,recbound,deferred,traceinfo,ptrs)}"
    puts "* appended to: #{src}"
  end  
  puts " #{"-"*78} "
end  

clangsources, clangflags, bplsources, rounds, delays, rest = parse_args()
src = C2S.clang_frontend( clangsources, clangflags, bplsources, $cleanup )
resolve_tm_operations( src )
inject_tm_harness( src, $cycle, $recbound, $deferred, $traceinfo, 
  # false ) 
 !clangsources.empty? )  
seq = C2S.delaybounding( src, rounds, delays )
C2S.verify( seq, rest, $cleanup, $graph )
cleanup( [src, seq] )
