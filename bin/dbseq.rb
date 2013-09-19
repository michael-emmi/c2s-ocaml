#!/usr/bin/env ruby

require 'colorize'
require 'optparse'
require 'ostruct'
require_relative 'prelude'
require_relative 'verify'

$MYVERSION = "0.1"

def c2s()
  err "cannot find c2s in executable path." if `which c2s`.empty?
  return "c2s"
end

def delay_bounding_seqentialization(src, options)
  seq = "#{File.basename(src,'.bpl')}.EQR.#{options.rounds}.#{options.delays}.bpl"
  puts "* c2s: #{src} => #{seq.blue}" unless options.quiet
  cmd = "#{c2s()} load #{src} seq-framework " \
    "delay-bounding #{options.rounds} #{options.delays} " \
    "async-to-seq-dfs " \
    "prepare #{options.verifier} " \
    "strip-internal-markers " \
    "print #{seq}"
  puts cmd if options.verbose
  err "could not translate." unless system(cmd)
  return seq
end

def dbseq_options(opts, options)
  options.c2s = []
  options.rounds = 1
  options.delays = 0

  opts.separator ""
  opts.separator "Sequentialization options:"

  opts.on("-r", "--rounds MAX", Integer, "The rounds bound (default 1)") do |r|
    options.rounds = r 
  end

  opts.on("-d", "--delays MAX", Integer, "The delay bound (default 0)") do |d|
    options.delays = d 
  end
  
  opts.on("-g", "--graph-of-trace", "generate a trace graph") do |g|
    options.graph = g
  end
end

# if this script is executing...
if __FILE__ == $0 then
  
  options = {}

  OptionParser.new do |opts|
    options = OpenStruct.new
    opts.banner = "usage: #{File.basename $0} SOURCE [options]"
    standard_options(opts, options)
    dbseq_options(opts, options)
    verify_options(opts, options)
  end.parse!

  # the rest of the command line
  err "Must specify a single Boogie source file." unless ARGV.size == 1
  src = ARGV[0]
  err "Source file '#{src}' does not exist." unless File.exists?(src)

  t0 = Time.now()

  # 1. concurrent to sequential translation
  seq = delay_bounding_seqentialization(src, options)

  # 2. verify the sequential code with Boogie
  verify(seq, options)

  # 3. remove temporary files
  File.delete( seq ) unless options.keep

  puts "#{File.basename $0} finished in #{(Time.now() - t0).round(2)}s." unless options.quiet
end
