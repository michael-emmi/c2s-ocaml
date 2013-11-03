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
  include DelayBounding
  include Verifier
  include Violin
  version 0.9
  
  run do
    err "Must specify a source file." unless ARGV.size > 0
    ARGV.each do |src|
      err "Source file '#{src}' does not exist." unless File.exists?(src)
    end
  
    t0 = Time.now()
    src = translate(ARGV)
    src = instrumentation(src)
    seq = sequentialize(src)
    verify(seq)
    File.delete( seq ) unless @keep

    puts "#{File.basename $0} finished in #{(Time.now() - t0).round(2)}s." unless @quiet
  end
end
