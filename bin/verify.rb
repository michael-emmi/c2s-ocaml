#!/usr/bin/env ruby

require_relative 'prelude'
require_relative 'boogie-trace-parser'

module Verifier
  
  attr_accessor :verifier, :timeout, :boogie_opts, :graph
  
  def options(opts)

    @verifier = :boogie_si
    @timeout = nil
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

    opts.on("-b", "--recursion-bound MAX", Integer, "The recursion bound (default -)") do |r|
      @boogie_opts << "/recursionBound:#{r}"
    end

    opts.on("-l", "--loop-unroll NUM", Integer, "The loop unrolling (default -)") do |n|
      @boogie_opts << "/loopUnroll:#{n}"
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
    
    case @verifier
    when :boogie_si
      warn "without specifying a /recursionBound, Boogie might not terminate." \
        unless @boogie_opts.index{|o| o =~ /\/recursionBound/}
          
      @boogie_opts << "/stratifiedInline:2"
      @boogie_opts << "/extractLoops"
      @boogie_opts << "/siVerbose:1" if @verbose

    when :boogie_fi
      warn "without specifying a /loopUnroll, Boogie might be imprecise." \
        unless @boogie_opts.index{|o| o =~ /\/loopUnroll/}

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
