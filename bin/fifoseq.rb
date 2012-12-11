#!/usr/bin/env ruby

MYVERSION = "0.1"
C2S = "#{File.dirname $0}/c2s"
BOOGIE = "Boogie"
CLEANUP = false
$graph = false

puts "FiFoSeq version #{MYVERSION}"

require 'C2S'
require 'scriptprelude'

def usage()
    puts "usage: fifoseq.rb <impl>.bpl <K>"
end

def prepare()
  sources, rest = ARGV.partition{|f| File.extname(f) == ".bpl"}
  phases, rest = rest.partition{|a| a =~ /\/phaseBound:[0-9]+/}
  delays, rest = rest.partition{|a| a =~ /\/delayBound:[0-9]+/}
  m2s, rest = rest.partition{|a| a =~ /\/multitosingle/}
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

  if phases.empty? then
  	puts "Using default /phaseBound:1."
  	phases = 1
  else
    phases = phases.first.sub(/\/phaseBound:([0-9]+)/, '\1').to_i
  end

  if delays.empty? then
  	puts "Using default /delayBound:0."
    delays = 0
  else
    delays = delays.first.sub(/\/delayBound:([0-9]+)/, '\1').to_i
  end

  m2s = !m2s.empty?  
  $graph = !graph.empty?
  rest = rest * " "

  src = "#{File.basename(sources.last,'.bpl')}.comp.bpl"
  puts "Combining [#{sources * ", "}] into #{src}." if sources.length > 1
  `cat #{sources.map{|s| escape(s)} * " "} > #{src}`
  
  puts " #{"-"*78} "
  return src, m2s, phases, delays, rest
end 

def cleanup( files )
  File.delete( *files ) if CLEANUP
end

src, m2s, phases, delays, rest = prepare()
seq = C2S.phasebounding( src, phases, delays, m2s )
C2S.verify( seq, rest, CLEANUP, $graph )
cleanup( [src, seq] )
