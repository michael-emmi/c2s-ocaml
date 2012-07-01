#!/usr/bin/env ruby

@version = "0.1"

c2s = "c2s"
boogie = "Boogie"
cleanup = false
rec_bound = 1

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

impl = bpl_source(ARGV[0])
# spec = cfg_source(ARGV[1])
delays = ARGV[1].to_i
rounds = ARGV[2].to_i
rec_bound = ARGV[3].to_i
if not ARGV[1] then
    puts "Please give a numeric bound."
    usage()
    exit
end

name = File.basename(impl,".bpl")

### Translate the specification grammar to a Presburger formula
# puts "Translating #{spec}.cfg to #{spec}.parikh.bpl"
# `#{c2s} #{spec} --cfg-to-presburger --print #{name}.parikh.bpl`

### Instrumentation for linearizability w.r.t. specification checking
# puts "Instrumenting #{impl}.bpl"
# `#{c2s} #{impl} --violin-instrument #{bound} --print #{name}.inst.bpl`
# `cat #{name}.parikh.bpl #{name}.inst.bpl > #{name}.violin.bpl`

seq = "#{name}.#{delays}-delay.bpl"

puts "Sequentializing #{impl} with #{delays}-delay translation."
puts "-- Rounds: #{rounds}"
puts "-- Delays: #{delays}"
`#{c2s} #{impl} --delay-bounding #{rounds} #{delays} --prepare-for-back-end --print #{seq}`

puts "Verifying #{seq} with Boogie..."
puts "-- StratifiedInline"
puts "-- ExtractLoops"
puts "-- RecursionBound: #{rec_bound}"
t0 = Time.now
cmd = "#{boogie} #{seq} /stratifiedInline:1 /extractLoops /recursionBound:#{rec_bound}"
puts "#{cmd}"
puts `#{cmd}`
puts "Finished in #{Time.now - t0}s."

# if cleanup then
#     File.delete( "#{src}.async.bpl" )
# end