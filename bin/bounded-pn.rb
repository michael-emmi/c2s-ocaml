#!/usr/bin/env ruby

c2s = "c2s"
boogie = "Boogie"
cleanup = false
@version = 0.1

puts "Bounded PN explorer, version #{@version}"

def usage()
    puts "usage: bounded-pn.rb <file>.spec /purseBound:_ ..."
end

# def pull(args, pattern, res)
#     match, rest = args.partition{|a| a =~ pattern}
#     match.first.sub(pattern, res)
#     return matc, rest
# end

sources, rest = ARGV.partition{|f| File.extname(f) == ".spec"}
purse, rest = rest.partition{|a| a =~ /\/purseBound:[0-9]+/}

if sources.length != 1 then
	puts "Please specify exactly one source file (.spec)."
	usage();
	exit -1
end

if purse.empty? then
	puts "Warning: Consider specifying a purse bound with /purseBound:_; using default 0."
	purse = 0
else
    purse = purse.first.sub(/\/purseBound:([0-9]+)/, '\1')
end

if rest.select{|a| a =~ /\/loopUnroll:/}.empty? then
    puts "Warning: consider specifying an unrolling bound with /loopUnroll:_."
end

rest = rest * " "

input = sources.last
src = File.basename(sources.last,".spec")

puts "Translating #{src}.spec to #{src}.purse.bpl"
`#{c2s} #{input} --pn-to-bpl #{purse} --print #{src}.purse.bpl`

puts "Verifying #{src}.purse.bpl"
puts `#{boogie} #{src}.purse.bpl #{rest}`

if cleanup then
    File.delete( "#{src}.async.bpl" )
end