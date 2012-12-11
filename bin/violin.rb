#!/usr/bin/env ruby

MYVERSION = "0.1"

C2S = "c2s"
BOOGIE = "Boogie"
$cleanup = true
$graph = false

puts "Violin version #{MYVERSION}"

require 'C2S'
require 'scriptprelude'

def usage()
    puts "usage: violin.rb <impl>.bpl /rounds:_ /delayBound:_"
end

def prepare()
  sources, rest = ARGV.partition{|f| File.extname(f) == ".bpl"}
  rounds, rest = rest.partition{|a| a =~ /\/rounds:[0-9]+/}
  delays, rest = rest.partition{|a| a =~ /\/delayBound:[0-9]+/}
  m2s, rest = rest.partition{|a| a =~ /\/multitosingle/}
  keep, rest = rest.partition{|a| a =~ /\/keepFiles/}
  graph, rest = rest.partition{|a| a =~ /\/graph(Of)?Trace/}

  if sources.empty? then
  	puts "Please specify at least one Boogie source file (.bpl)."
  	exit -1
  end
  
  sources.each do |f|
    if not File.exists?(f) then
      puts "Cannot find file `#{f}'."
      exit -1
    end
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

  m2s = !m2s.empty?
  $cleanup = keep.empty?
  $graph = !graph.empty?
  rest = rest * " "

  src = "#{File.basename(sources.last,'.bpl')}.comp.bpl"
  puts "Combining [#{sources * ", "}] into #{src}." if sources.length > 1
  `cat #{sources.map{|s| escape(s)} * " "} > #{src}`
  
  puts " #{"-"*78} "
  return src, rounds, delays, rest
end 

def cleanup( files )
  File.delete( *files ) if $cleanup
end

src, rounds, delays, rest = prepare()
seq = C2S.delaybounding( src, rounds, delays )
C2S.verify( seq, rest, $cleanup, $graph )
cleanup( [src, seq] )
