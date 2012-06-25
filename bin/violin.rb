#!/usr/bin/env ruby

@version = "0.1"

c2s = "c2s"
boogie = "Boogie"
cleanup = false

puts "Violin version #{@version}"

def usage()
    puts "usage: violin.rb <impl>.bpl <spec>.cfg <K>"
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
spec = cfg_source(ARGV[1])
bound = ARGV[2].to_i
if not ARGV[2] then
    puts "Please give a numeric bound."
    usage()
    exit
end

name = File.basename(impl,".bpl")

puts "Translating #{spec}.cfg to #{spec}.parikh.bpl"
`#{c2s} #{spec} --cfg-to-presburger --print #{name}.parikh.bpl`

puts "Instrumenting #{impl}.bpl"
`#{c2s} #{impl} --violin-instrument #{bound} --print #{name}.inst.bpl`
`cat #{name}.parikh.bpl #{name}.inst.bpl > #{name}.violin.bpl`

# if cleanup then
#     File.delete( "#{src}.async.bpl" )
# end