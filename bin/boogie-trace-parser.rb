#!/usr/bin/env ruby

require_relative 'prelude'

module BoogieTraceParser
  attr_accessor :has_rounds
  
  def options(opts)
    opts.on("--[no]concurrency", "Process c2s concurrency, e.g. rounds, delays.") do |b|
      @has_rounds = b
    end
  end
  
  def dot
    err "cannot find 'dot' in executable path." if `which dot`.empty?
    return "dot"
  end
  
  def trace2dot(tracefile)
    tempfile( dotfile = File.basename(tracefile) + ".dot" )
    File.open(dotfile,'w') do |f|
      f.write( graph_of(IO.readlines(tracefile)) )
    end
    return dotfile
  end
  
  def dot2svg(dotfile)
    svgfile = File.basename(dotfile) + ".svg"
    cmd = "#{dot} -Tsvg #{dotfile} -o#{svgfile}"
    err "could not generate SVG image" unless system(cmd)
    return svgfile
  end
  
  def opensvg(svgfile)
    err "could not open graph image" unless system("open #{svgfile}")
  end
  
  def showtrace(tracefile)
    opensvg( dot2svg( trace2dot(tracefile) ) )
  end
  
  attr_accessor :lines, :tree

  def node
    "n#{@unique += 1}"
  end

  def following(pattern)
    @lines.shift until @lines.empty? || m = @lines.first =~ pattern
    err "expecting #{pattern}" if @lines.empty?
    @lines.shift
    yield m if block_given?
  end

  def see(pattern)
    m = @lines.shift.match(pattern)
    err "expecting #{pattern}" unless m
    yield m if block_given?
  end

  def graph_of(lines)
    @lines = lines
    @tree = []
    @unique = 0
    
    following(/Boogie program verifier version/)
    see(/This assertion might not hold./)
    see(/Execution trace:/)    
    procedure
    see(/Boogie program verifier finished/)

    "digraph G { \
      \n  node [shape = record];
      \n  #{tree * "\l  "} \
    \n}"
  end
  
  def procedure(name = nil)
    me = node
    stmts = []
    round_known = !name
    
    until !(line = @lines.shift.chomp) ||
      (name && line =~ /Inlined call to procedure .* ends/) ||
      (!name && line.empty?) do
      
      next if line.match /Inlined call to procedure (.*) begins/ do |m|
        stmts << "call #{m[1]}"
        child = procedure m[1]
        tree << "#{me} -> #{child};"
      end
      
      next if line.match /value = T@\$mop!val!(\d+)/ do |m|

        # TODO change SMACK
        # kind = see(/value = (.*)/){|m| m[1] == 0 ? :read : :write}
        op = m[1].to_i > 0 ? "=&gt;" : ":="

        addr = see(/value = (.*)/){|m| m[1].to_i}
        val  = see(/value = (.*)/){|m| m[1].to_i}
        stmts << "M[#{addr}] #{op} #{val}"
      end
      
      next if line.match /value = (.*)/ do |m|
        if round_known then
          stmts << "val: #{m[1]}"
        else
          stmts << " | ROUND #{m[1]} "
          round_known = true
        end
      end
      
      next if line.match /.*\.bpl\(\d+,\d+\): ~yield/ do
        round_known = false
      end
      
      next if line.match /.*\.bpl\(\d+,\d+\): .*/ do
        true
      end
      
      err "unexpected line: #{line}"
    end
    
    tree << "#{me} [label=\"{#{name || "top"} #{stmts * '\\l'}\\l }\"];"
    return me
  end
  
end

if __FILE__ == $0 then
  include Tool
  include BoogieTraceParser
  version 0.3
  
  run do 
    err "Must specify a single Boogie source file." unless ARGV.size == 1
    tracefile = ARGV[0]
    err "Source file '#{tracefile}' does not exist." unless File.exists?(tracefile)
    showtrace(tracefile)
  end
end