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
  attr_accessor :step_by_step
  
  def options(opts)
    opts.on("--[no-]step-by-step", "Visualize of each step of the trace.") do |g|
      @step_by_step = g
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

    if @step_by_step then
      id = 0
      complete_log(unscramble(IO.readlines(tracefile))).each do |m|
        dotfiles << tempfile( "#{File.basename(tracefile,".trace")}.step#{id += 1}.dot" )
        File.open(dotfiles.last,'w') do |f|
          f.write( graph_of_log_entry(m) )
        end
      end
    end
    
    return dotfiles
  end
  
  def render_graphs(dotfiles)
    svgfiles = []
    dotfiles.each do |dotfile|
      print "generating graph: #{dotfile.abbreviate(40)}\r" if dotfiles.size > 1
      svgfiles << File.basename(dotfile,".dot") + ".svg"
      cmd = "#{dot} -Tsvg #{dotfile} -o#{svgfiles.last}"
      puts cmd.bold if @verbose
      err "could not generate SVG image" unless system(cmd)
    end
    
    htmlfile = File.basename(dotfiles.first,".dot") + ".html"

    File.open(htmlfile,'w') do |f|
      f.write "<html><head></head><body>"
      svgfiles.each do |svg|
        f.write "<object id='object' type='image/svg+xml' data='#{svg}'></object>"
      end
      f.write "</body></html>"
    end

    return htmlfile
  end
  
  def open_image(htmlfile)
    err "could not open graph image" unless system("open #{htmlfile}")
  end
  
  def showtrace(tracefile)
    open_image( render_graphs( trace2dot(tracefile) ) )
  end
  
  class Parser
    def initialize
      @procs = []
      @seqs = []
      @rounds = []
      @bpl_locs = []
      @src_locs = []
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
    def curr_source_code ; get_source_code(get_source_loc(@bpl_locs[-1])) end
    
    def push(proc_name, seq_no, round_no)
      @procs << proc_name
      @seqs << seq_no
      @rounds << round_no
      @bpl_locs << nil
      @src_locs << nil
    end
    
    def pop ; [@procs, @seqs, @rounds, @bpl_locs, @src_locs].each &:pop end
    
    def parse(lines)
      block_exprs = []

      loop do
        line = lines.shift.chomp
        break unless line
        next if line.empty?
        next if line.match /Boogie program verifier version .*/
        next if line.match /This assertion might not hold./
      
        if line.match /Execution trace:/ then
          push("top", 0, 0)
          @callbacks[:trace_begin].each {|b| b.call}
          next
      
        elsif line.match /Boogie program verifier finished/ then
          @callbacks[:trace_end].each {|b| b.call}
          pop
          break

        elsif m = line.match(/Inlined call to procedure (.*) begins/) then
          seq = lines.delete_first_match(/value = (.*)/){|m| m[1].to_i}
          round = lines.delete_first_match(/value = (.*)/){|m| m[1].to_i}
          is_async = seq != @seqs.last

          @callbacks[:procedure_begin].each {|b| b.call(m[1], is_async, seq, round)}
          push(m[1],seq,round)
          next

        elsif m = line.match(/Inlined call to procedure (.*) ends/) then
          @callbacks[:procedure_end].each {|b| b.call(m[1])}
          pop
          next

        elsif m = line.match(/(\S+\.bpl)\((\d+),(\d+)\): (\S*)/) then
          @bpl_locs[-1] = { file: m[1], line: m[2].to_i, col: m[3].to_i, label: m[4] }
          @src_locs[-1] = get_source_loc @bpl_locs.last

          block_exprs = recorded_exprs(@bpl_locs.last, lines)
          if m[4] =~ /~yield/
            round = lines.delete_first_match(/value = (.*)/){|m| m[1].to_i}
            @callbacks[:yield].each {|b| b.call(round)}
            @rounds[-1] = round
          end
          @callbacks[:block].each {|b| b.call(m[1], m[2].to_i, m[3].to_i, m[4])}
          next

        elsif m = line.match(/value = T@\$mop!val!(\d+)/) then
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
          next

        elsif m = line.match(/value = (.*)/) then
          rec = block_exprs.shift || {val: m[1]}
          @callbacks[:recorded_value].each {|b| b.call(rec)}
          next

        else
          warn "unexpected line: #{line}"
        end
      end
    end

    private
    
    def each_bpl_line(bpl_file)
      return unless block_given?
      begin
        File.new(bpl_file,'r').each_line do |line|
          break unless yield line
        end
      rescue
        warn "could not read BPL source file '#{src_loc[:file]}'"
      end
    end
    
    def get_bpl_code(bpl_loc)
      line_no = 0
      each_bpl_line(bpl_loc[:file]) do |line|
         next true if line_no += 1 < bpl_loc[:line]
         
      end
    end
    
    def get_source_code(src_loc)
      return nil unless src_loc

      code = nil
      begin
        line_no = 0
        File.new(src_loc[:file],'r').each_line do |line|
          next if (line_no += 1) < src_loc[:line]
          code = line
          break
        end
      rescue
        warn "could not read (C) source file '#{src_loc[:file]}'"
      end
      return code
    end
    
    def get_source_loc(bpl_loc)
      src_loc = nil
      begin
        line_no = 0
        File.new(bpl_loc[:file],'r').each_line do |line|
          next if (line_no += 1) < bpl_loc[:line]
          break if line =~ /^[$]bb\d+:/
          break if line.match /\{:sourceloc (.*), (\d+), (\d+)\}/ do |m|
            src_loc = { file: m[1], line: m[2].to_i, col: m[3].to_i }
          end
        end
      rescue
        warn "could not open BPL source file '#{bpl_loc[:file]}'"
      end
      return src_loc
    end
    
    def recorded_exprs(bpl_loc, lines)
      num_vals = lines.index{|l| l !~ /value = .*/}
      line_no = 0
      exprs = []
      begin
        File.new(bpl_loc[:file],'r').each_line do |line|
          break unless exprs.size < num_vals
          next if (line_no += 1) < bpl_loc[:line]
          break if line =~ /^[$]bb\d+:/
          line.match /call\s+boogie_si_record_(\S+)\s*\((.*)\)\s*;/ do |m|
            val = lines[exprs.size].match(/value = (.*)/){|m| m[1].gsub(/[() ]/,"")}
            exprs << {expr: m[2], type: m[1], val: val}
          end
        end
      rescue
        warn "could not open BPL source file '#{bpl_loc[:file]}'"
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
    
    global_addrs = get_global_addrs( get_bpl_files(lines))
  
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
      bin << {
        task: parser.curr_seq_no,
        code: parser.curr_source_code,
        step: is_async ?
          "async/#{seq} #{procname}" :
          "call #{procname}"
      }
    end
    
    parser.on_procedure_end do |procname|
      bin = log(unscrambled, parser.curr_round_no, parser.curr_seq_no)
      bin << {task: parser.curr_seq_no, step: "return from #{procname}"}
    end
    
    parser.on_block do |bplfile, line, column, label|
    end
    
    parser.on_memory_operation do |kind, addr, val|
      bin = log(unscrambled, parser.curr_round_no, parser.curr_seq_no)
      addr = "#{global_addrs[addr]}:#{addr}" if global_addrs.include? addr
      bin << {
        task: parser.curr_seq_no, 
        code: parser.curr_source_code,
        step: (kind == :read) ?
          "read M[#{addr}] = #{val}" :
          "write M[#{addr}] := #{val}"
      }
    end
    
    parser.on_recorded_value do |rec|
      bin = log(unscrambled, parser.curr_round_no, parser.curr_seq_no)
      bin << {
        task: parser.curr_seq_no,
        code: parser.curr_source_code,
        step: rec[:expr] ?
          "echo #{rec[:expr]}:#{rec[:type]} = #{rec[:val]}" :
          "echo #{rec[:val]}"
      }
    end
    
    parser.on_yield do |next_round|
      bin = log(unscrambled, parser.curr_round_no, parser.curr_seq_no)
      # next_bin = log(unscrambled, next_round, parser.curr_seq_no)
      # bin << "yield"
      # next_bin << "resume #{parser.curr_proc_name}/#{parser.curr_seq_no}"
      bin << {task: parser.curr_seq_no, code: parser.curr_source_code, step: "yield"}
    end
    
    parser.parse(lines)
    unscrambled.flatten!
    unscrambled.compact!
    return unscrambled
  end
  
  def complete_log(unscrambled_trace)
    log = []
    stacks = {}
    stacks[active_task = 0] = []
    memory = {}
    
    unscrambled_trace.each do |step|
      
      active_task = step[:task]
      current_step = step[:step]
      current_code = step[:code]
      
      if m = current_step.match(/call (.*)/) then
        stacks[active_task] << m[1]

      elsif current_step.match(/return/) then
        stacks[active_task].pop
      
      elsif m = current_step.match(/async\/(.*) (.*)/) then
        task_id = m[1].to_i
        stacks[task_id] = [m[2]]

      elsif current_step.match(/read/) then
        
      elsif m = current_step.match(/write M\[(.*)\] := (.*)/) then
        memory[m[1]] = m[2]
      end
      
      stacks_clone = {}
      stacks.each do |idx,stack|
        stacks_clone[idx] = stack.clone
      end

      log << {
        active_task: active_task, 
        current_step: current_step,
        source_code: current_code,
        stacks: stacks_clone,
        memory: memory.clone
      }
    end
    return log
  end
  
  def graph_of_log_entry(log_entry)

    unique_node_id = 0
    active_node = "active"

    stack_nodes = []
    stack_edges = []
    mem_names = {}
    mem_nodes = []
    mem_edges = []
    
    code = log_entry[:source_code]
    
    stack_nodes << "#{active_node} [label=\" #{code || ""} #{log_entry[:current_step]} \", color=none]"
    
    log_entry[:stacks].each_pair do |idx,stack|
      stack_node = "n#{unique_node_id += 1}"
      task_node = "n#{unique_node_id += 1}"
      stack_nodes << "#{stack_node} [label=\" { #{stack.reverse * " | "} } \"]"
      is_active = idx == log_entry[:active_task]
      stack_nodes << "#{task_node} [label=\" TASK #{idx} \", shape=#{is_active ? "egg" : "none"} ]"
      stack_edges << "#{active_node} -> #{stack_node} [style=invis]"
      stack_edges << "#{stack_node} -> #{task_node} [dir=back]"
    end
    
    curr_addr = curr_val = nil
    log_entry[:current_step].match(/M\[(.*)\] :?= (.*)/) do |m|
      curr_addr = m[1]
      curr_val = m[2]
    end
    
    log_entry[:memory].each do |addr,val|
      left = mem_names[addr] || mem_names[addr] = "n#{unique_node_id += 1}"
      right = mem_names[val] || mem_names[val] = "n#{unique_node_id += 1}"
      mem_edges << "#{left} -> #{right};"
    end
    mem_names.each do |name,node|
      highlighted = name == curr_addr || name == curr_val
      mem_nodes << "#{node} [label=\"#{name}\", shape=#{ highlighted ? "octagon" : "oval" }];"
    end
    
    "digraph G {
      subgraph cluster_entry {
        subgraph cluster_memory {
          graph [style=dotted, ordering=out];
          node [shape = oval];
          label = \"MEMORY\";
          #{mem_nodes * "\n"}
          #{mem_edges * "\n"}
        }
        subgraph cluster_stacks {
          graph [style=dotted, ordering=out];
          node [shape = record];
          label = \"TASKS\";
          #{stack_nodes * "\n"}
          #{stack_edges * "\n"}
        }
      }
    }\n"
    
  end
  
  def get_bpl_files(lines)
    bplfiles = Set.new
    lines.each do |line|
      line.match /(\S*.bpl)\(\d+,\d+\):/ do |m|
        bplfiles << m[1]
      end
    end
    return bplfiles.to_a
  end
  
  def get_global_addrs(bplfiles)
    addrs = {}
    bplfiles.each do |bplfile|
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