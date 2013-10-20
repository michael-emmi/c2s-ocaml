#!/usr/bin/env ruby

require 'colorize'
require 'optparse'
require 'ostruct'
require_relative 'prelude'
require_relative 'clang2bpl'
require_relative 'dbseq'
require_relative 'verify'

$MYVERSION = "0.1"
options = {}

def violin_options(opts, options)
  
  options.barriers = 0

  opts.separator ""
  opts.separator "Encoding options:"
  
  opts.on("-b", "--barriers NUM", Integer, "The barrier bound (default 0)") do |k|
    options.barriers = k
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
  violin_options(opts, options)

  options.clang << "-g"
end.parse!

def violin_instrumentation( src, options )
  seq = "#{File.basename(src,'.bpl')}.VIOLIN.#{options.barriers}.bpl"
  puts "* c2s/LIN: #{src} => #{seq.blue}" unless options.quiet
  cmd = "#{c2s()} load #{src} " \
    "violin-instrument #{options.barriers} " \
    "print #{seq}"
  puts cmd if options.verbose
  err "could not instrument." unless system(cmd)
  return seq
end  

t0 = Time.now()

# 1. translate Clang to Boogie
src = translate_clang_to_bpl( ARGV, options )

# 2. perform the linearizability-to-reachability instrumentation
src = violin_instrumentation( src, options )

# 3. concurrent to sequential translation
seq = delay_bounding_seqentialization(src, options)

# 4. verify the sequential code with Boogie
verify(seq, options)

# 5. remove temporary files
File.delete( seq ) unless options.keep

puts "#{File.basename $0} finished in #{(Time.now() - t0).round(2)}s." unless options.quiet

