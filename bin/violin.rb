#!/usr/bin/env ruby

require_relative 'prelude'
require_relative 'clang2bpl'
require_relative 'dbseq'
require_relative 'verify'

module Violin  
  
  attr_accessor :barriers

  def options(opts)
  
    @barriers = nil

    opts.separator ""
    opts.separator "Encoding options:"
  
    opts.on("-b", "--barriers NUM", Integer, "The barrier bound (default nil)") do |k|
      @barriers = k
    end
    
  end
  
  def po_instrumentation(src)
    seq = "#{File.basename(src,'.bpl')}.VIOLIN.bpl"
    puts "* c2s/LIN: #{src} => #{seq.blue}" unless @quiet
    cmd = "#{c2s} load #{src} violin print #{seq}"
    puts cmd.bold if @verbose
    err "could not instrument." unless system(cmd)
    return seq
  end

  def barrier_instrumentation(src)
    seq = "#{File.basename(src,'.bpl')}.VIOLIN.#{@barriers}.bpl"
    puts "* c2s/LIN: #{src} => #{seq.blue}" unless @quiet
    cmd = "#{c2s()} load #{src} " \
      "violin-barriers #{@barriers} " \
      "print #{seq}"
    puts.bold cmd if @verbose
    err "could not instrument." unless system(cmd)
    return seq
  end
  
  def instrumentation(src)
    if @barriers then
      barrier_instrumentation(src)
    else
      po_instrumentation(src)
    end
  end
end

if __FILE__ == $0 then
  include Tool
  include Clang2Bpl
  include Violin
  include DelayBounding
  include Verifier
  include BoogieTraceParser
  version 0.9
  
  run do
    err "Must specify a source file." unless ARGV.size > 0
    ARGV.each do |src|
      err "Source file '#{src}' does not exist." unless File.exists?(src)
    end

    # tempfile( src = translate(ARGV) )
    src = ARGV.first

    tempfile( src = instrumentation(src) )
    tempfile( seq = sequentialize(src) )
    verify(seq)
  end
end
