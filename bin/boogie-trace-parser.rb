#!/usr/bin/env ruby

require_relative 'prelude'

class Array
  def delete_first_match(pattern)
    begin
      idx = index{|x| x =~ pattern}
      m = delete_at(idx).match(pattern)
      if block_given? then yield m else m[0] end
    rescue
      nil
    end
  end
end

class String
  def abbreviate(max_len = 20)
    if size > max_len then
      "#{self[0..max_len/2-1]}..#{self[-(max_len/2-1)..-1]}"
    else
      self
    end
  end
end

module BoogieTraceParser
  attr_accessor :memory_graphs
  
  def options(opts)
    opts.on("--[no-]memory-graphs", "Generate graphs of memory.") do |g|
      @memory_graphs = g
    end
  end
  
  def dot
    err "cannot find 'dot' in executable path." if `which dot`.empty?
    return "dot"
  end
  
  def trace2dot(tracefile)
    dotfiles = []
    dotfiles << tempfile( File.basename(tracefile,".trace") + ".dot" )
    File.open(dotfiles.last,'w') do |f|
      f.write( build_graph(IO.readlines(tracefile)) )
    end

    if @memory_graphs then
      id = 0
      memory_log(unscramble(IO.readlines(tracefile))).each do |m|
        dotfiles << tempfile( "#{File.basename(tracefile,".trace")}.mem#{id += 1}.dot" )
        File.open(dotfiles.last,'w') do |f|
          f.write( graph_of_memory(m) )
        end
      end
    end
    
    return dotfiles
  end
  
  def dot2svg(dotfiles)
    svgfiles = []
    dotfiles.each do |dotfile|
      print "generating graph: #{dotfile.abbreviate(40)}\r" if dotfiles.size > 1
      svgfiles << File.basename(dotfile,".dot") + ".svg"
      cmd = "#{dot} -Tsvg #{dotfile} -o#{svgfiles.last}"
      puts cmd if @verbose
      err "could not generate SVG image" unless system(cmd)
    end
    return svgfiles
  end
  
  def opensvg(svgfiles)
    err "could not open graph image" unless system("open #{svgfiles.first}")
  end
  
  def showtrace(tracefile)
    opensvg( dot2svg( trace2dot(tracefile) ) )
  end
  
  class Parser
    def initialize
      @procs = []
      @seqs = []
      @rounds = []
      @callbacks = {
        trace_begin: [], trace_end: [],
        procedure_begin: [], procedure_end: [],
        block: [], yield: [], memory_operation: [], recorded_value: []
      }
    end
    def on_trace_begin(&block) ; @callbacks[:trace_begin] << block end
    def on_trace_end(&block) ; @callbacks[:trace_end] << block end
    def on_procedure_begin(&block) ; @callbacks[:procedure_begin] << block end
    def on_procedure_end(&block) ; @callbacks[:procedure_end] << block end
    def on_block(&block) ; @callbacks[:block] << block end
    def on_yield(&block) ; @callbacks[:yield] << block end
    def on_memory_operation(&block) ; @callbacks[:memory_operation] << block end
    def on_recorded_value(&block) ; @callbacks[:recorded_value] << block end
    def curr_proc_name ; @procs.last end
    def curr_seq_no ; @seqs.last end
    def curr_round_no ; @rounds.last end

    def parse(lines)
    
      block_exprs = []

      loop do
        line = lines.shift.chomp
        break unless line
        next if line.empty?
        next if line.match /Boogie program verifier version .*/
        next if line.match /This assertion might not hold./
      
        next if line.match /Execution trace:/ do
          @procs << "top"
          @seqs << 0
          @rounds << 0
          @callbacks[:trace_begin].each {|b| b.call}
        end
      
        break if line.match /Boogie program verifier finished/ do
          @callbacks[:trace_end].each {|b| b.call}
          @procs.pop
          @seqs.pop
          @rounds.pop
        end
              
        next if line.match /Inlined call to procedure (.*) begins/ do |m|
          seq = lines.delete_first_match(/value = (.*)/){|m| m[1].to_i}
          round = lines.delete_first_match(/value = (.*)/){|m| m[1].to_i}
          is_async = seq != @seqs.last

          @callbacks[:procedure_begin].each {|b| b.call(m[1], is_async, seq, round)}
          @procs << m[1]
          @seqs << seq
          @rounds << round
        end
        
        next if line.match /Inlined call to procedure (.*) ends/ do |m|
          @callbacks[:procedure_end].each {|b| b.call(m[1])}
          @procs.pop
          @seqs.pop
          @rounds.pop
        end      
      
        next if line.match /(\S+\.bpl)\((\d+),(\d+)\): (\S*)/ do |m|
          block_exprs = recorded_exprs(m[1], m[2].to_i, lines)
          if m[4] =~ /~yield/
            round = lines.delete_first_match(/value = (.*)/){|m| m[1].to_i}
            @callbacks[:yield].each {|b| b.call(round)}
            @rounds[-1] = round
          end
          @callbacks[:block].each {|b| b.call(m[1], m[2].to_i, m[3].to_i, m[4])}
        end
      
        next if line.match /value = T@\$mop!val!(\d+)/ do |m|
          unless lines.size > 3 && lines.take(3).all? {|line| line =~ /value = .*/} then
            warn "unexpected memory operation format."
          else
            kind = lines.delete_first_match(/value = (.*)/){|m| m[1].to_i == 0 ? :read : :write}
            addr = lines.delete_first_match(/value = (.*)/){|m| m[1].gsub(/[() ]/,"").to_i}
            val = lines.delete_first_match(/value = (.*)/){|m| m[1].gsub(/[() ]/,"").to_i}

            # addr = @addrs[addr] || addr
            # val = "#{val} (#{@addrs[val]})" if @addrs[val]
            @callbacks[:memory_operation].each {|b| b.call(kind, addr, val)}
          end
          true
        end
      
        next if line.match /value = (.*)/ do |m|
          rec = block_exprs.shift || {val: m[1]}
          @callbacks[:recorded_value].each {|b| b.call(rec)}
        end
      
        warn "unexpected line: #{line}"
      end
    end

    private  
    def recorded_exprs(bplfile, bplline, lines)
      num_vals = lines.index{|l| l !~ /value = .*/}
      line_no = 0
      exprs = []
      begin
        File.new(bplfile,'r').each_line do |line|
          break unless exprs.size < num_vals
          next if (line_no += 1) < bplline
          break if line =~ /^[$]bb\d+:/
          line.match /call\s+boogie_si_record_(\S+)\s*\((.*)\)\s*;/ do |m|
            val = lines[exprs.size].match(/value = (.*)/){|m| m[1].gsub(/[() ]/,"")}
            exprs << {expr: m[2], type: m[1], val: val}
          end
        end
      rescue
        warn "could not open BPL source file '#{bplfile}'"
      end
      return exprs
    end
  end

  def build_graph(lines)
    unique_node_id = 0
    node_ids = []
    stmtss = []
    tree = []
    
    parser = Parser.new
    
    parser.on_trace_begin do
      node_ids << "n#{unique_node_id += 1}"
      stmtss << []
    end
    
    parser.on_trace_end do
      tree << "#{node_ids.pop} [label=\"{top | #{stmtss.pop * '\l'}}\"];"
    end
    
    parser.on_procedure_begin do |procname, is_async, seq, round|
      stmtss.last << "#{"async" if is_async} call #{procname} #{" : #{seq}" if is_async}"
      node_ids << "n#{unique_node_id += 1}"
      stmtss << []
      stmtss.last << "ROUND #{round}"
    end
    
    parser.on_procedure_end do |procname|
      seq = parser.curr_seq_no
      child = node_ids.pop
      parent = node_ids.last
      tree << "#{parent} -> #{child};"
      tree << "#{child} [label=\"{#{procname} : #{seq} | #{stmtss.pop * '\l'}\\l}\"];"
    end
    
    parser.on_block do |bplfile, line, column, label|
      
    end
    
    parser.on_memory_operation do |kind,addr,val|
      if kind == :read
        stmtss.last << "read M[#{addr}] = #{val}"
      else
        stmtss.last << "write M[#{addr}] := #{val}"
      end
    end
    
    parser.on_recorded_value do |rec|
      if rec[:expr] && rec[:type]
        stmtss.last << "echo #{rec[:expr]}:#{rec[:type]} = #{rec[:val]}"
      else
        stmtss.last << "echo #{rec[:val]}"
      end
    end
    
    parser.on_yield do |next_round|
      stmtss.last << "| ROUND #{next_round}"
    end
    
    parser.parse(lines)
    
    "digraph G {
       node [shape = record];
       #{tree * "\n"}
     }\n"
  end

  def unscramble(lines)
    unscrambled = []
  
    def log(struct, i, j)
      struct[i] ||= []
      struct[i][j] ||= []
      struct[i][j]
    end  
    
    parser = Parser.new
    
    parser.on_trace_begin do
    end
    
    parser.on_trace_end do
    end
    
    parser.on_procedure_begin do |procname, is_async, seq, round|
      bin = log(unscrambled, parser.curr_round_no, parser.curr_seq_no)
      bin << "call #{procname}"
    end
    
    parser.on_procedure_end do |procname|
      bin = log(unscrambled, parser.curr_round_no, parser.curr_seq_no)
      bin << "return from #{procname}"
    end
    
    parser.on_block do |bplfile, line, column, label|
    end
    
    parser.on_memory_operation do |kind, addr, val|
      bin = log(unscrambled, parser.curr_round_no, parser.curr_seq_no)
      if kind == :read
        bin << "read M[#{addr}] = #{val}"
      else
        bin << "write M[#{addr}] := #{val}"
      end
    end
    
    parser.on_recorded_value do |rec|
    end
    
    parser.on_yield do |next_round|
      bin = log(unscrambled, parser.curr_round_no, parser.curr_seq_no)
      next_bin = log(unscrambled, next_round, parser.curr_seq_no)
      bin << "yield"
      next_bin << "resume #{parser.curr_proc_name}/#{parser.curr_seq_no}"
    end
    
    unscrambled.each do |r|
      
    end

    parser.parse(lines)    
    return unscrambled.flatten.compact
  end
  
  def memory_log(unscrambled_trace)
    log = []
    memory = {}
    log << memory.clone
    unscrambled_trace.each do |line|
      line.match(/write M\[(.*)\] := (.*)/) do |m|
        memory[m[1]] = m[2]
        log << memory.clone
      end
    end
    log
  end
  
  def graph_of_memory(memory)
    unique_node_id = 0
    names = {}
    nodes = []
    edges = []
    memory.each do |addr,val|
      left = names[addr] || names[addr] = "n#{unique_node_id += 1}"
      right = names[val] || names[val] = "n#{unique_node_id += 1}"
      edges << "#{left} -> #{right};"
    end
    names.each do |name,node|
      nodes << "#{node} [label=\"#{name}\"];"
    end
    
    "digraph G {
      node [shape = record];
      #{nodes * "\n"}
      #{edges * "\n"}
    }\n"
  end
  
  def global_addrs
    addrs = {}
    bplfile = nil
    @lines.each do |line|
      break if line.match /(\S*.bpl)\(\d+,\d+\):/ do |m|
        bplfile = m[1]
      end
    end    
    unless bplfile
      warn "could not determine BPL source file."
    else 
      begin
        File.new(bplfile).each_line do |line|
          line.match /axiom (\S+) == (-\d+);/ do |m|
            addrs[m[2].to_i] = m[1]
          end
        end
      rescue
        warn "could not open BPL source file '#{bplfile}'"
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
    err "Must specify a single Boogie trace file." unless ARGV.size == 1
    tracefile = ARGV[0]
    err "Source file '#{tracefile}' does not exist." unless File.exists?(tracefile)
    showtrace(tracefile)
  end
end