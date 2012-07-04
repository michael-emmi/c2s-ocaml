#!/usr/bin/env ruby

@version = "0.1"

c2s = "c2s"
boogie = "Boogie"
cleanup = false

puts "Violin version #{@version}"

def usage()
    puts "usage: violin.rb <impl>.bpl <K>"
end

def check_file(file,kind,ext)
    if not file \
    or not File.exists? file \
    or not File.extname(file) == ".#{ext}"
    then
        puts "Please give a #{kind} source file -- .#{ext} extension."
        usage()
        exit
    end
end

def bpl_source(file)
    check_file(file,"Boogie source","bpl")
    return file
end

def cfg_source(file)
    check_file(file,"context-free grammar","cfg")
    return file
end

sources, rest = ARGV.partition{|f| File.extname(f) == ".bpl"}
delays, rest = rest.partition{|a| a =~ /\/delayBound:[0-9]+/}
rounds, rest = rest.partition{|a| a =~ /\/rounds:[0-9]+/}

if sources.empty? then
	puts "Please specify at least one Boogie source file (.bpl)."
	exit -1
end

if delays.empty? then
	puts "Please specify a delay bound with /delayBound:_."
	exit -1
end

if rounds.empty? then
	puts "Please specify the number of rounds with /rounds:_."
	exit -1
end

delays = delays.first.sub(/\/delayBound:([0-9]+)/, '\1')
rounds = rounds.first.sub(/\/rounds:([0-9]+)/, '\1')
rest = rest * " "

name = File.basename(sources.last,".bpl")

comp = "#{name}.comp.bpl"

`cat #{sources * " "} > #{comp}`

### Translate the specification grammar to a Presburger formula
# puts "Translating #{spec}.cfg to #{spec}.parikh.bpl"
# `#{c2s} #{spec} --cfg-to-presburger --print #{name}.parikh.bpl`

### Instrumentation for linearizability w.r.t. specification checking
# puts "Instrumenting #{impl}.bpl"
# `#{c2s} #{impl} --violin-instrument #{bound} --print #{name}.inst.bpl`
# `cat #{name}.parikh.bpl #{name}.inst.bpl > #{name}.violin.bpl`

seq = "#{name}.#{delays}-delay.bpl"

puts "Sequentializing #{name} with #{delays}-delay translation."
puts "-- Rounds: #{rounds}"
puts "-- Delays: #{delays}"
`#{c2s} #{comp} --delay-bounding #{rounds} #{delays} --prepare-for-back-end --print #{seq}`

puts "Verifying #{seq} with Boogie..."
puts "-- StratifiedInline"
puts "-- ExtractLoops"
puts "-- and: #{rest}"
t0 = Time.now
cmd = "#{boogie} #{seq} /stratifiedInline:1 /extractLoops #{rest} \
	/errorLimit:1 /errorTrace:2"
# other interesting flags: 
# /errorLimit:1 -- only one error (per procedure)
# /errorTrace:2 -- include all trace labels in error output
puts "#{cmd}"
puts `#{cmd}`
puts "Finished in #{Time.now - t0}s."

# if cleanup then
#     File.delete( "#{src}.async.bpl" )
# end