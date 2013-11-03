#!/usr/bin/env ruby

require_relative 'prelude'
require_relative 'clang2bpl'
require_relative 'dbseq'
require_relative 'verify'
require_relative 'serencoding'

module Trombone
  
  attr_accessor :update, :cycle
  
  def options(opts)
  
    @update = :deferred
    @cycle = 2

    opts.separator ""
    opts.separator "Encoding options:"
  
    opts.on("-u", "--update KIND", [:direct, :deferred], 
      "Write semantics (direct or deferred)") do |u|
      @update = u
    end

    opts.on("-c", "--cycle MAX", Integer, "The cycle bound (default 2)") do |r|
      @cycle = c
    end
  end

  def resolve_tm_operations( src )  
    text = File.read(src)
    pat = /[_A-Za-z0-9]*(TMInit|TXBegin|TXRead|TXWrite|TXCommit|TXAbort)[_A-Za-z0-9]*/
    File.open(src, 'w') do |f|
      f.puts text.gsub(pat,'\1')
    end  
  end
  
  # Inject the cycle-size specific axioms
  def inject_tm_harness( src )
    File.open(src, 'a') do |f|
      f.puts "#{harnessXXX()}"
    end  
  end  
end

if __FILE__ == $0 then
  include Tool
  include Clang2Bpl
  include DelayBounding
  include Verifier
  include Trombone
  version 0.9
  
  run do
    err "Must specify a source file." unless ARGV.size > 0
    ARGV.each do |src|
      err "Source file '#{src}' does not exist." unless File.exists?(src)
    end
    
    # @clang_opts << "-g"
    # @smack_opts << "-mem-mod-impls"
  
    t0 = Time.now()
    src = translate( ARGV )

    resolve_tm_operations(src)
    inject_tm_harness(src)  
    puts "* TM operations resolved, harness appended to #{src.blue}" unless @quiet

    seq = sequentialize(src)
    verify(seq)
    File.delete( seq ) unless @keep
    puts "#{File.basename $0} finished in #{(Time.now() - t0).round(2)}s." unless @quiet
  end
end
