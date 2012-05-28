#!/usr/bin/env ruby

c2s = "c2s"
boogie = "Boogie"
cleanup = false

input = ARGV[0]

if not input \
or not File.exists? input \
or not File.extname(input) == ".bpl"
then
    puts "Please give a Boogie source file."
    exit
end

src = File.basename( input, ".bpl" )

puts "Translating #{src}.bpl to #{src}.async.bpl"
`#{c2s} -esc-async true #{input} > #{src}.async.bpl`

puts "Verifying #{src}.async.bpl"
puts `#{boogie} #{src}.async.bpl`

if cleanup then
    File.delete( "#{src}.async.bpl" )
end