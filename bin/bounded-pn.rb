#!/usr/bin/env ruby

c2s = "c2s"
boogie = "Boogie"
cleanup = false

purse_bound=2
loop_unroll=3

input = ARGV[0]

if not input \
or not File.exists? input \
or not File.extname(input) == ".spec"
then
    puts "Please give a Petri net source file -- .spec extension."
    exit
end

src = File.basename( input, ".spec" )

puts "Translating #{src}.spec to #{src}.purse.bpl"
`#{c2s} #{input} --pn-to-bpl #{purse_bound} --print #{src}.purse.bpl`

puts "Verifying #{src}.purse.bpl"
puts `#{boogie} #{src}.purse.bpl /loopUnroll:#{loop_unroll}`

if cleanup then
    File.delete( "#{src}.async.bpl" )
end