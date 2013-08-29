#!/usr/bin/env ruby

require 'colorize'
require 'optparse'
require 'ostruct'

$MYVERSION = "0.2"

def err( msg )
  puts "Error: #{msg}".red
  exit -1
end

def clang()
  err "cannot find 'clang' in executable path." if `which clang`.empty?
  return "clang"
end

def llvmlink()
  err "cannot find LLVM's 'llvm-link' in executable path." if `which llvm-link`.empty?
  return "llvm-link"
end

def llvmopt()
  err "cannot find LLVM's 'opt' in executable path." if `which opt`.empty?
  return "opt"
end

def smacklib()
  # search the (Ruby) library path for the smack dynamic library
  $:.each do |path|
    ["smack.dylib", "smack.so", "smack.dll"].each do |lib|
      return File.join(path,lib) if File.exists?(File.join(path,lib))
    end
  end
  err "cannot find smack.{dylib,so,dll} in library path."
end

def smack()
  return "#{llvmopt()} -load #{smacklib()} " \
  "-internalize --mem2reg -die -lowerswitch -bpl_print -debug-only=bpl -debug -disable-output"
end

def translate_clang_to_bpl(sources, options)
  llvmbcs = []
  bpl = options.outfile \
    || sources.map{|f| File.basename(f,File.extname(f))} * "+" + ".bpl"

  # 1. compile the sources one by one to LLVM bytecode
  sources.each do |src|
    llvmbcs << bc = "#{File.basename(src,File.extname(src))}.bc"
    puts "* Clang: #{src} => #{bc.light_blue}" unless options.quiet
    cmd = "#{clang()} #{options.clang * " "} -g -c #{src} -emit-llvm -o #{bc}"
    puts cmd if options.verbose
    err "failed to compile #{src}." unless system(cmd)
  end

  # 2. link the bytecodes together
  bc = File.basename(bpl,'.bpl') + '.bc'
  if llvmbcs.length > 1 then
    puts "* link: #{llvmbcs * ", "} => #{bc.light_blue}" unless options.quiet
    cmd = "#{llvmlink()} -o #{bc} #{llvmbcs * " "}"
    puts cmd if options.verbose
    err "failed to link #{llvmbcs * ", "}." unless system(cmd)
    llvmbcs << bc
  end

  # 3. translate the bytecode to Boogie
  puts "* SMACK: #{bc} => #{bpl.blue}" unless options.quiet
  cmd = "#{smack()} #{options.smack * " "} #{bc} 2> #{bpl}"
  puts cmd if options.verbose
  err "failed to translate bytecode to Boogie." unless system(cmd)

  # 4. remove temporary files
  File.delete( *llvmbcs ) unless options.keep

  return bpl
end

# if this script is executing...
if __FILE__ == $0 then

  options = {}

  OptionParser.new do |opts|
    options = OpenStruct.new
    options.outfile = nil
    options.clang = []
    options.smack = ["-mem-mod-impls"]
  
    opts.banner = "usage: #{File.basename $0} CLANG-SOURCES [options]"
  
    opts.on("-o", "--output-file FILE", "Specify the output file name") do |f|
      options.outfile = f
    end
  
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
  
    opts.separator ""
    opts.separator "Clang options:"

    opts.on("-DSYMBOL", "Define C preprocessor SYMBOL") do |d|
      options.clang << "-D#{d}" 
    end
  
    opts.separator ""
    opts.separator "SMACK options:"

    opts.on("-l", "--source-loc-syms", "Add source location annotations") do |s|
      options.smack << "-source-loc-syms"
    end

    opts.on("-m", "--memory-model MODEL", [:flat, :twodim], "Select memory model (flat, twodim)") do |m|
      options.smack << "-mem-mod=#{m}"
    end

    opts.on("--memory-model-debug", "Memory model debugging instrumentation") do |m|
      options.smack << "-mem-mod-dbg"
    end
  
    opts.on("--[no-]memory-model-impls", "Use implementations of memory procedures") do |m|
      options.smack.delete("-mem-mod-impls") unless m
    end
  
    opts.on("--[no-]memory-model-asserts", "Enable memory model assertions") do |m|
      options.smack << "-no-mem-mod-asserts" unless m
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

  # the rest of the command line in ARGV

  t0 = Time.now()

  translate_clang_to_bpl(ARGV, options)

  puts "#{File.basename $0} finished in #{(Time.now() - t0).round(2)}s." unless options.quiet
  
end
