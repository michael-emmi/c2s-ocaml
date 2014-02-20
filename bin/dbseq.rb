#!/usr/bin/env ruby

require_relative 'prelude'
require_relative 'verify'

module DelayBounding
  
  attr_accessor :rounds, :delays

  def options(opts)
    
    @rounds = nil
    @delays = 0

    opts.separator ""
    opts.separator "Sequentialization options:"

    opts.on("-r", "--rounds MAX", Integer, "The rounds bound (default 1)") do |r|
      @rounds = r 
    end

    opts.on("-d", "--delays MAX", Integer, "The delay bound (default 0)") do |d|
      @delays = d 
    end
  end
  
  def c2s
    err "cannot find c2s in executable path." if `which c2s`.empty?
    return "c2s"
  end

  def sequentialize(src)
    @rounds ||= @delays + 1
    seq = "#{File.basename(src,'.bpl')}.EQR.#{@rounds}.#{@delays}.bpl"
    puts "* c2s: #{src} => #{seq.blue}" unless @quiet
    cmd = "#{c2s} load #{src} seq-framework " \
      "delay-bounding #{@rounds} #{@delays} " \
      "async-to-seq-dfs " \
      "prepare #{@verifier} " \
      "strip-internal-markers " \
      "print #{seq}"
    puts cmd.bold if @verbose
    err "could not translate." unless system(cmd)
    return seq
  end
end

if __FILE__ == $0 then
  include Tool
  include DelayBounding
  include Verifier
  include BoogieTraceParser
  version 1.0
  
  run do
    err "Must specify a single Boogie source file." unless ARGV.size == 1
    src = ARGV[0]
    err "Source file '#{src}' does not exist." unless File.exists?(src)
  
    tempfile( seq = sequentialize(src) )
    verify(seq)
  end
end
