#!/usr/bin/env ruby

require_relative 'prelude'

module BoogieTraceParser
  attr_accessor :blah
  
  def options(opts)
    opts.on("--[no]blah", "Blah blah BLAH") do |b|
      @blah = b
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
    @addrs = global_addrs
    
    following(/Boogie program verifier version/)
    see(/This assertion might not hold./)
    rounds = trace

    # rounds.each_index {|r| puts "ROUND #{r}", indent(rounds[r])}

    "digraph G { \
      \n  node [shape = record];
      \n  #{@tree * "\n  "} \
    \n}"
  end
  
  def indent(trace)
    indent_level = 0
    trace.map do |line|
      line = (" " * indent_level) + line
      indent_level = 0 if line =~ /YIELD/
      indent_level += 2 if line =~ /call/
      indent_level -= 2 if line =~ /return/ && indent_level > 1
      line
    end
  end
  
  def trace
    see(/Execution trace:/)
    _, _, rounds = procedure
    return rounds
  end

  def step(str, *dest)
    dest.each do |d|
      d << str
    end
  end
  
  def sequence_rounds(before, after)
    0.upto([before.size, after.size].max - 1).map do |i|
      (before[i] || []).concat(after[i] || [])
    end
  end

  def procedure(name = nil)
    me = node
    stmts = []
    seq = name ? nil : 0
    round = name ? nil : 0
    rounds = [[]]
    after = [[]]
    has_yielded = false
    
    until !(line = @lines.shift.chomp) ||
      (!name && line.empty?) ||
      (!name && line =~ /Boogie program verifier finished/) do
        
      break if line.match /Inlined call to procedure .* ends/ do
        step "return", rounds[round]
      end
        
      next if line.match /Inlined call to procedure (.*) begins/ do |m|        
        is_async = next_val.to_i != seq

        step "#{is_async ? "async" : "call"} #{m[1]}", stmts, rounds[round]
        child, their_round, their_rounds = procedure m[1]
        @tree << "#{me} -> #{child};"
        
        if is_async then
          step "begin #{m[1]}", after[round]
          after = sequence_rounds(after, their_rounds)
        else
          round = their_round
          rounds[ round ] ||= []
          step "begin #{m[1]}", rounds[round]
          rounds = sequence_rounds(rounds, their_rounds)
        end

        true
      end
      
      next if line.match /value = T@\$mop!val!(\d+)/ do |m|

        kind = see(/value = (.*)/){|m| m[1].to_i == 0 ? :read : :write}
        addr = see(/value = (.*)/){|m| m[1].gsub(/[() ]/,"").to_i}
        val  = see(/value = (.*)/){|m| m[1].gsub(/[() ]/,"").to_i}
        
        addr = @addrs[addr] || addr
        val = "#{val} (#{@addrs[val]})" if @addrs[val]

        if kind == :read
          step "read M[#{addr}] = #{val}", stmts, rounds[round]
        else
          step "write M[#{addr}] := #{val}", stmts, rounds[round]
        end
      end
      
      next if line.match /value = (.*)/ do |m|
        rec = @exprs.shift
        
        if name && !seq then
          seq = rec[:val].to_i
        
        elsif name && !round then
          round = rec[:val].to_i
          rounds[ round ] ||= []
          step "|ROUND #{round}", stmts
          step "RESUME #{name}", rounds[round] if has_yielded

        else
          step "echo #{rec[:expr]}:#{rec[:type]} = #{rec[:val]}", stmts, rounds[round]
        end
        true
      end
      
      next if line.match /(\S+\.bpl)\((\d)+,(\d)+\): ~yield/ do |m|
        @block = {file: m[1], line: m[2].to_i, col: m[3].to_i}
        @exprs = recorded_exprs
        step "YIELD", rounds[round]
        has_yielded = true
        round = nil
      end
      
      next if line.match /(\S+\.bpl)\((\d+),(\d+)\): .*/ do |m|
        @block = {file: m[1], line: m[2].to_i, col: m[3].to_i}
        @exprs = recorded_exprs
        true
      end
      
      err "unexpected line: #{line}"
    end
    
    @tree << "#{me} [label=\"{#{name || "top|"} #{stmts * '\l'} }\"];"
    return me, round, sequence_rounds(rounds, after)
  end
  
  def next_val
    val = nil
    @lines.each do |line|
      break if line.match /value = (.*)/ do |m|
        val = m[1]
      end      
    end
    val
  end
  
  def recorded_exprs
    num_vals = @lines.index{|l| l !~ /value = .*/}
    
    line_no = 0
    exprs = []
    File.new(@block[:file],'r').each_line do |line|
      break unless exprs.size < num_vals
      next if (line_no += 1) < @block[:line]
      line.match /call\s+boogie_si_record_(\S+)\s*\((.*)\)\s*;/ do |m|
        val = @lines[exprs.size].match(/value = (.*)/){|m| m[1].gsub(/[() ]/,"")}
        exprs << {expr: m[2], type: m[1], val: val}
      end
    end
    return exprs
  end
  
  def global_addrs
    bplfile = nil
    @lines.each do |line|
      break if line.match /(\S*.bpl)\(\d+,\d+\):/ do |m|
        bplfile = m[1]
      end
    end
    err "Cannot determine BPL source file." unless bplfile

    addrs = {}
    File.new(bplfile).each_line do |line|
      line.match /axiom (\S+) == (-\d+);/ do |m|
        addrs[m[2].to_i] = m[1]
      end
    end
    return addrs
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