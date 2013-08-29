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

options = {}

OptionParser.new do |opts|
  
  options = OpenStruct.new
  options.clang = ["-g"]
  options.smack = ["-mem-mod-impls"]
  options.c2s = []
  options.verifier = "Boogie-SI"
  options.boogie = []
  options.update = :deferred
  options.cycle = 2
  options.rounds = 1
  options.delays = 0
  options.recursion = nil
  
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
  opts.separator "Clang options:"

  opts.on("-DSYMBOL", "Define C preprocessor SYMBOL") do |d|
    options.clang << "-D#{d}" 
  end
  
  opts.separator ""
  opts.separator "Encoding options:"
  
  opts.on("-u", "--update KIND", [:direct, :deferred], 
    "Write semantics (direct or deferred)") do |u|
    options.update = u
  end

  opts.on("-c", "--cycle MAX", Integer, "The cycle bound (default 2)") do |r|
    options.cycle = c
  end
  
  opts.on("-r", "--rounds MAX", Integer, "The rounds bound (default 1)") do |r|
    options.rounds = r 
  end
  
  opts.on("-d", "--delays MAX", Integer, "The delay bound (default 0)") do |d|
    options.delays = d 
  end

  opts.separator ""
  opts.separator "Verifier options:"
  
  opts.on("--verifier NAME", String, ["Boogie-SI", "Boogie-FI"], "The verification engine") do |v|
    options.verifier = v
  end
  
  opts.on("-b", "--recursion-bound MAX", Integer, "The recursion bound (default oo)") do |b|
    options.recursion = b
    options.boogie << "/recursionBound:#{b}"
  end
  
  opts.on("-l", "--loop-unroll NUM", Integer, "Loop unrolling (default 0)") do |l|
    options.loopunroll = l
    options.boogie << "/loopUnroll:#{l}"
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

