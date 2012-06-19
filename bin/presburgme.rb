#!/usr/bin/env ruby

c2s = "c2s"
boogie = "Boogie"
cleanup = false

purse_bound=2
loop_unroll=3

input = ARGV[0]

if not input \
or not File.exists? input \
or not File.extname(input) == ".cfg"
then
    puts "Please give a context-free grammar source file -- .cfg extension."
    exit
end

src = File.basename( input, ".cfg" )

puts "Translating #{src}.cfg to #{src}.parikh.bpl"
`#{c2s} #{input} --cfg-to-presburger --print #{src}.parikh.bpl`

puts "Here is your Presburger formula:"
puts `cat #{src}.parikh.bpl | sed "s/axiom\\(.*\\);/\\1/"`

# if cleanup then
#     File.delete( "#{src}.async.bpl" )
# end