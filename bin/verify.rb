#!/usr/bin/env ruby

require_relative 'prelude'
require_relative 'boogie-trace-parser'

module Verifier
  
  attr_accessor :verifier, :timeout, :boogie_opts, :graph
  
  def options(opts)

    @verifier = :boogie_si
    @timeout = nil
    @unroll = nil
    @boogie_opts = []
    @graph = false
    
    opts.separator ""
    opts.separator "Verifier options:"
  
    opts.on("--verifier NAME", [:boogie_si, :boogie_fi], 
            "Select verifier (boogie_si, boogie_fi)") do |v|
      @verifier = v
    end
  
    opts.on("-t", "--timeout TIME", Integer, "The prover timeout (default -)") do |t|
      @boogie_opts << "/timeLimit:#{t}"
    end

    opts.on("-u", "--unroll MAX", Integer, "The loop-unroll/recursion bound (default -)") do |u|
      @unroll = u
    end
    
    opts.on("-g", "--graph-of-trace", "generate a trace graph") do |g|
      @graph = g
    end
  end

  def boogie
    ['Boogie','boogie','Boogie.exe','boogie.exe'].each do |b|
      return "#{b}" if not `which #{b}`.empty?
    end
    err "cannot find 'Boogie' in executable path."
  end
  
  def verify(src)
    puts "* Boogie: #{src}" unless @quiet
    
    warn "without specifying an unroll bound, Boogie may not terminate" \
      unless @unroll
    
    case @verifier
    when :boogie_si
      @boogie_opts << "/stratifiedInline:2"
      @boogie_opts << "/extractLoops"
      @boogie_opts << "/recursionBound:#{@unroll}" if @unroll
      @boogie_opts << "/weakArrayTheory"
      @boogie_opts << "/siVerbose:1" if @verbose

    when :boogie_fi
      @boogie_opts << "/loopUnroll:#{@unroll}" if @unroll

    else
      err "invalid back-end: #{@verifier}"
    end
       
    @boogie_opts << "/errorLimit:1"
    @boogie_opts << "/errorTrace:2"

    cmd = "#{boogie()} #{src} #{@boogie_opts * " "}"
    puts cmd if @verbose
    t = Time.now
    
    output = task do
      `#{cmd}`
    end
        
    cleanup = []
    if not $?.success? then
      err "problem with Boogie: #{output}"
    else
      if @graph && output =~ /[1-9]\d* errors?/ then
        puts "Rendering error trace.." unless @quiet
        File.open("#{src}.trace",'w'){|f| f.write(output) }
        showtrace "#{src}.trace"
      else
        if @quiet then
          puts output.lines.select{|l| l =~ /[0-9]* verified/}[0]
        else
          puts output.lines.reject{|l| l.strip.empty?} * ""
        end
      end
    end 
    File.delete( *cleanup ) unless @keep
    puts "Boogie finished in #{Time.now - t}s." unless @quiet
  end
end

if __FILE__ == $0 then
  include Tool
  include Verifier
  include BoogieTraceParser
  version 1.0
  
  run do
    err "Must specify a single Boogie source file." unless ARGV.size == 1
    src = ARGV[0]
    err "Source file '#{src}' does not exist." unless File.exists?(src)
    verify(src)
  end

end
