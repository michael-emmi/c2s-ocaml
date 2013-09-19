#!/usr/bin/env ruby

require 'colorize'
require 'optparse'
require 'ostruct'
require_relative 'prelude'
require_relative 'clang2bpl'
require_relative 'dbseq'
require_relative 'verify'
require_relative 'serencoding'

$MYVERSION = "0.1"

def trombone_options(opts, options)
  
  options.update = :deferred
  options.cycle = 2

  opts.separator ""
  opts.separator "Encoding options:"
  
  opts.on("-u", "--update KIND", [:direct, :deferred], 
    "Write semantics (direct or deferred)") do |u|
    options.update = u
  end

  opts.on("-c", "--cycle MAX", Integer, "The cycle bound (default 2)") do |r|
    options.cycle = c
  end
  
  opts.separator ""
  opts.separator "Other options:"
  
  opts.on("-g", "--graph-of-trace", "generate a trace graph") do |g|
    options.graph = g
  end   
  
end

OptionParser.new do |opts|  
  options = OpenStruct.new  
  opts.banner = "usage: #{File.basename $0} SOURCE [options]"

  standard_options(opts, options)
  clang2bpl_options(opts, options)
  dbseq_options(opts, options)
  verify_options(opts, options)
  trombone_options(opts, options)

  options.clang << "-g"
  options.smack << "-mem-mod-impls"
end.parse!

def resolve_tm_operations( src, options )  
  text = File.read(src)
  pat = /[_A-Za-z0-9]*(TMInit|TXBegin|TXRead|TXWrite|TXCommit|TXAbort)[_A-Za-z0-9]*/
  File.open(src, 'w') do |f|
    f.puts text.gsub(pat,'\1')
  end  
end
  
# Inject the cycle-size specific axioms
def inject_tm_harness( src, options )
  File.open(src, 'a') do |f|
    f.puts "#{harnessXXX(options)}"
  end  
end  

t0 = Time.now()

# 1. translate Clang to Boogie
src = translate_clang_to_bpl( ARGV, options )

# 2. resolve the TM operations, inject the test harness
resolve_tm_operations(src, options)
inject_tm_harness(src, options)  
puts "* TM operations resolved, harness appended to #{src.blue}" unless options.quiet

# 3. concurrent to sequential translation
seq = delay_bounding_seqentialization(src, options)

# 4. verify the sequential code with Boogie
verify(seq, options)

# 5. remove temporary files
File.delete( seq ) unless options.keep

puts "#{File.basename $0} finished in #{(Time.now() - t0).round(2)}s." unless options.quiet

