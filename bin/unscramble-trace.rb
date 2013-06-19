#!/usr/bin/env ruby

MYVERSION = "0.1"

IDENT = /[A-Za-z0-9$_][A-Za-z0-9.$_-]*/

def parse(lines)
  round = [0]
  seq = [0]
  level = [0]
  trace = []
  
  putin = lambda {|line|
      trace[round.last] = [] if trace[round.last].nil?
      trace[round.last][seq.last] = [] if trace[round.last][seq.last].nil?
      trace[round.last][seq.last] << (" " * (level.last*4)) + line.strip
  }
  
  while not lines.empty? do    
    
    if lines.first =~ /Execution trace:/ then
      putin.call(lines.first)
      round << 0
      seq << 0
      level << 1
      lines.shift
      
    elsif lines.first =~ /Boogie program verifier finished/ then
      round.pop
      seq.pop
      level.pop
      
      round[-1] = 9999
      seq[-1] = 9999
      level[-1] = 0
      putin.call(lines.first)
      lines.shift
      
    elsif m = lines[0].match(/Inlined call to procedure (#{IDENT}) begins/) then
      if m2 = lines[1].match(/: ~entry/) \
      and m3 = lines[2].match(/value = (\d+)/) \
      and m4 = lines[3].match(/value = (\d+)/) 

        if m4[1].to_i > seq.last then
          putin.call("Async call to procedure #{m[1]}")
        end
        
        round << m3[1].to_i
        seq << m4[1].to_i
        if seq[-1] == seq[-2] then
          putin.call(lines[0])
          level << level.last + 1
        else
          level << 0
          putin.call(lines[0])
          level[-1] = 1
        end
        lines.slice!(0..3)
      end
      
    elsif m = lines[0].match(/: ~yield/) \
    and m1 = lines[1].match(/value = (\d+)/) \
    and m2 = lines[2].match(/value = (\d+)/) then
      if m1[1].to_i > round.last then
        putin.call("Yielding task #{m2[1].to_i}")
      end
      round[-1] = m1[1].to_i
      seq[-1] = m2[1].to_i
      putin.call("Resuming task #{seq.last}")
      lines.slice!(0..2)      

    elsif m = lines[0].match(/Inlined call to procedure #{IDENT} ends/) then
      level[-1] = level.last - 1
      putin.call(lines.shift)
      round.pop
      seq.pop
      level.pop
      
    elsif m = lines[0].match(/Boogie program verifier finished/) then
      warn "Unexpected end of trace." if round.length > 1
      break

    else
      putin.call(lines.shift)
    end
  end
  
  trace.compact!
  trace.each do |r|
    r.compact!
  end
  return trace
end
  
def print(trace)
  trace.each do |round|
    round.each do |task|
      task.each do |line|
        puts line unless line.empty?
      end
      puts "#{"-"*34} NEXT TASK #{"-"*35}" unless round.last == task
    end
    puts "#{"="*34} END ROUND #{"="*35}" unless trace.last == round
  end
end

input_file = ARGV[0]
output_file = "#{File.basename(input_file)}-unscrambled.bpltrace"
lines = IO.readlines(input_file)
trace = parse(lines)
print(trace)
