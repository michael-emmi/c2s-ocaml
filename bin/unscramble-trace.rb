#!/usr/bin/env ruby

MYVERSION = "0.1"

IDENT = /[A-Za-z0-9$_][A-Za-z0-9.$_-]*/

def parse(lines)
  round = [0]
  seq = [0]
  level = [0]
  traces = [[]]
  
  putin = lambda {|line|
    traces.last[round.last] = [] if traces.last[round.last].nil?
    traces.last[round.last][seq.last] = [] if traces.last[round.last][seq.last].nil?
    traces.last[round.last][seq.last] << (" " * (level.last*4)) + line.strip
  }
    
  while not lines.empty? do
    
    if lines.first =~ /Execution trace:/ then
      round = [0]
      seq = [0]
      level = [0]
      traces << []
      putin.call(lines.first)
      level = [1]
      
    elsif lines.first =~ /Boogie program verifier finished/ then
      traces << []
      round = [0]
      seq = [0]
      level = [0]
      putin.call(lines.first)
      
    elsif p = lines.first.match(/Inlined call to procedure (#{IDENT}) begins/) \
    and lines.length >= 4 \
    and lines[1].match(/: ~entry/) \
    and r = lines[2].match(/value = (\d+)/) \
    and s = lines[3].match(/value = (\d+)/) \
    then
      if s[1].to_i > seq.last then
        putin.call("Async call to procedure #{p[1]}")
      end
      round << r[1].to_i
      seq << s[1].to_i
      if seq[-1] == seq[-2] then
        putin.call(lines[0])
        level << level.last + 1
      else
        level << 0
        putin.call(lines[0])
        level[-1] = 1
      end
      lines.shift(3)
      
    elsif lines.first =~ /Inlined call to procedure (#{IDENT}) begins/ then
      round << round.last
      seq << seq.last
      putin.call(lines[0])
      level << level.last + 1
      
    elsif lines.first.match(/~yield/) \
    and lines.length >= 3 \
    and r = lines[1].match(/value = (\d+)/) \
    and s = lines[2].match(/value = (\d+)/) \
    then      
      if r[1].to_i > round.last then
        putin.call("Yielding task #{s[1].to_i}")
      end
      round[-1] = r[1].to_i
      seq[-1] = s[1].to_i
      putin.call("Resuming task #{seq.last}")
      lines.shift(2)
      
    elsif lines.first =~ /Inlined call to procedure #{IDENT} ends/ then
      level[-1] = level.last - 1
      putin.call(lines.first)
      round.pop
      seq.pop
      level.pop

    else
      putin.call(lines.first)
    end
    
    lines.shift
  end
  
  traces.compact!
  traces.each do |trace|
    trace.compact!
    trace.each do |round|
      round.compact!
    end
  end
  return traces
end
  
def print(traces)
  traces.each do |trace|
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
end

trace = parse(ARGF.read.lines)
print(trace)
