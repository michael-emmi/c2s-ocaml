#!/usr/bin/env ruby

MYVERSION = "0.1"
C2S = "#{File.dirname $0}/c2s"
BOOGIE = "Boogie"
CLEANUP = true

# puts "Boogie Trace Parser version #{MYVERSION}"

def usage()
    puts "usage: boogie-trace-parser.rb .."
end

BOOGIE_SIG = /Boogie program verifier version ([0-9.]+)/
TRACE_SIG = /\(\d+,\d+\):/
TRACE_START = /Execution trace:/
BOOGIE_END = /Boogie program verifier finished/

IDENT = /[A-Za-z0-9_][A-Za-z0-9.$_-]*/
NUMVAL = /(-?\d+)/
TVAL = /T@(#{IDENT})!val!(\d+)/
VAL = /#{NUMVAL}|#{TVAL}/

INTRAPROC_STEP = /(.*)\.bpl\(\d+,\d+\): #{IDENT}/
VALUE_STEP = /value = (#{VAL})/

CALL_BEGIN = /Inlined call to procedure (#{IDENT}) begins/
CALL_END = /Inlined call to procedure #{IDENT} ends/

DELAY = /DELAY/

def top(lines)
  if not (lines.delete_at(0) =~ BOOGIE_SIG) then
    return nil
  else
    while t = trace(lines) do
      return trace_to_graph(t)
    end
  end
end

$idx = 0
def new_node()
  $idx = $idx + 1
  return "n#{$idx}"
end

def trace_to_graph(t)
  n = new_node()
  g = []
  step_to_graph(t,nil,[],g)
  "digraph G { \
    \n  node [shape = record];
    \n  #{g*"\n  "} \
  \n}"
end

def step_to_graph(s,m,vals,g)
  if s[:proc] then
    n = new_node()
    vs = []
    
    s[:trace].each do |t|
      step_to_graph(t,n,vs,g)
    end
    
    g << "#{m} -> #{n};" if m
    g << "#{n} [label=\"{#{s[:proc]}|#{vs*'\\n'}}\"];"
  else
    vals << clean_val(s)
  end
end

def clean_val(v)
  if m = v.match(TVAL) then
    "#{m[1]}:#{m[2]}"
  elsif m = v.match(NUMVAL) then
    "#{m[1]}"
  else
    v
  end
end

def trace(lines)
  if lines.length < 2 ||
    !(lines[0] =~ TRACE_SIG) ||
    !(lines[1] =~ TRACE_START) then
    return nil
  else
    lines.delete_at(0)
    lines.delete_at(0)
    return procedure("TOP", lines)
  end
end

def procedure(p, lines)
  t = []
  while step(t, lines) do
    
  end
  return { :proc => p, :trace => t }
end
  
def step(t, lines)
  if lines.empty? then
    return nil
  end
  
  line = lines.delete_at(0)
  
  if line.strip.empty? then
    return "skip"
  
  elsif m = line.match( INTRAPROC_STEP ) then
    if line =~ DELAY then
      t << "*delay*"
    end
    return "step"
    
  elsif m = line.match( CALL_BEGIN ) then
    t << procedure( m[1], lines )
    return "call"
    
  elsif m = line.match( CALL_END ) then
    return nil
    
  elsif m = line.match( VALUE_STEP ) then
    t << m[1]
    return "val"
    
  elsif m = line.match( BOOGIE_END ) then
    return nil
    
  else
    warn "Unexpected line: #{line}"
    return "skip"
  end
end

def print(t)
  
end

input_file = ARGV[0]
dot_file = File.basename(input_file) + ".dot"
pdf_file = File.basename(input_file) + ".pdf"
g = top( IO.readlines(input_file) )
File.open(dot_file,'w') {|f| f.write(g)}
`dot -Tpdf #{dot_file} -o#{pdf_file} && open #{pdf_file}`
File.delete(*[dot_file, pdf_file])
