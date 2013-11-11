#!/usr/bin/env ruby

require_relative 'prelude'
require_relative 'clang2bpl'
require_relative 'dbseq'
require_relative 'verify'

module Violin  
  
  attr_accessor :barriers

  def options(opts)
  
    @barriers = 0

    opts.separator ""
    opts.separator "Encoding options:"
  
    opts.on("-b", "--barriers NUM", Integer, "The barrier bound (default 0)") do |k|
      @barriers = k
    end
  end

  def instrumentation(src)
    seq = "#{File.basename(src,'.bpl')}.VIOLIN.#{@barriers}.bpl"
    puts "* c2s/LIN: #{src} => #{seq.blue}" unless @quiet
    cmd = "#{c2s()} load #{src} " \
      "violin-instrument #{@barriers} " \
      "print #{seq}"
    puts cmd if @verbose
    err "could not instrument." unless system(cmd)
    return seq  end  
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
  
    tempfile( src = translate(ARGV) )
    tempfile( src = instrumentation(src) )
    tempfile( seq = sequentialize(src) )
    verify(seq)
  end
end
