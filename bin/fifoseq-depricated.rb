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

def phase_bounding_seqentialization(src, options)
  seq = "#{File.basename(src,'.bpl')}.FiFoSeq.#{options.phases}.#{options.delays}.bpl"
  puts "* c2s: #{src} => #{seq.blue}" unless options.quiet
  cmd = "#{c2s()} #{src} --seq-framework --phase-bounding #{options.phases} #{options.delays} --prepare-for-back-end --print #{seq}"
  puts cmd if options.verbose
  err "could not translate." unless system(cmd)
  return seq
end

# if this script is executing...
if __FILE__ == $0 then
  
  options = {}

  OptionParser.new do |opts|
    options = OpenStruct.new
    options.c2s = []
    options.boogie = []
    options.phases = 1
    options.delays = 0
    options.m2s = false
  
    opts.banner = "usage: #{File.basename $0} SOURCE [options]"
  
    opts.on("-v", "--[no-]verbose", "Run verbosely") do |v|
      options.verbose = v
      options.quiet = !v
    end
  
    opts.on("-q", "--[no-]quiet", "Run very quietly") do |q|
      options.quiet = q
      options.verbose = !q
    end

    opts.on("-k", "--[no-]keep-files", "Don't delete intermediate files") do |v|
      options.keep = v
    end

    opts.on("-g", "--graph-of-trace", "generate a trace graph") do |g|
      options.graph = g
    end
  
    opts.separator ""
    opts.separator "Sequentialization options:"

    opts.on("-p", "--phases MAX", Integer, "The phase bound (default 1)") do |p|
      options.phases = p
    end
  
    opts.on("-d", "--delays MAX", Integer, "The delay bound (default 0)") do |d|
      options.delays = d 
    end
  
    opts.on("-m2s", "--multi-to-single", "Multi-to-single processor reduction") do |m|
      options.m2s = m
    end
  
    opts.separator ""
    opts.separator "Boogie options:"

    opts.on("-b", "--recursion-bound MAX", Integer, "The recursion bound (default ??)") do |r|
      options.boogie << "/recursionBound:#{r}"
    end
  
    opts.separator ""
    opts.separator "Generic options:"
  
    opts.on_tail("-h", "--help", "Show this message") do
      puts opts
      exit
    end

    opts.on_tail("--version", "Show version") do
      puts "#{File.basename $0} version #{$MYVERSION}"
      exit
    end
  end.parse!

  # the rest of the command line
  err "Must specify a single Boogie source file." unless ARGV.size == 1
  src = ARGV[0]
  err "Source file '#{src}' does not exist." unless File.exists?(src)

  t0 = Time.now()

  # 1. concurrent to sequential translation
  seq = phase_bounding_seqentialization(src, options)

  # 2. verify the sequential code with Boogie
  verify(seq, options)

  # 3. remove temporary files
  File.delete( seq ) unless options.keep

  puts "#{File.basename $0} finished in #{(Time.now() - t0).round(2)}s." unless options.quiet
end
