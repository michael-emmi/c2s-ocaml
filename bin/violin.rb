#!/usr/bin/env ruby

@version = "0.1"

c2s = "c2s"
boogie = "mono ~/Code/Tools/boogie-source/Binaries/Boogie.exe"
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

impl = bpl_source(ARGV[0])
# spec = cfg_source(ARGV[1])
bound = ARGV[1].to_i
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

puts "Sequentializing #{impl} with #{bound}-delay translation."
`#{c2s} #{impl} --delay-bounding #{bound} --prepare-for-back-end --print #{name}.#{bound}-delay.bpl`

puts "Verifying #{name}.#{bound}-delay.bpl with Boogie..."
t0 = Time.now
puts `#{boogie} #{name}.#{bound}-delay.bpl /stratifiedInline:1 /extractLoops`
puts "Boogie finished in #{Time.now - t0}s."

# if cleanup then
#     File.delete( "#{src}.async.bpl" )
# end