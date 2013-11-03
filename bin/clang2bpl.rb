#!/usr/bin/env ruby

require_relative 'prelude'

module Clang2Bpl
  
  attr_accessor :outfile, :clang_opts, :smack_opts

  def options(opts)
    
    @outfile = nil
    @clang_opts = []
    @smack_opts = ["-mem-mod-impls"]
  
    opts.on("-o", "--output-file FILE", "Specify the output file name") do |f|
      @outfile = f
    end
  
    opts.separator ""
    opts.separator "Clang options:"

    opts.on("-DSYMBOL", "Define C preprocessor SYMBOL") do |d|
      @clang_opts << "-D#{d}" 
    end

    opts.separator ""
    opts.separator "SMACK options:"

    opts.on("-l", "--source-loc-syms", "Add source location annotations") do |s|
      @smack_opts << "-source-loc-syms"
    end

    opts.on("-m", "--memory-model MODEL", [:flat, :twodim], "Select memory model (flat, twodim)") do |m|
      @smack_opts << "-mem-mod=#{m}"
    end

    opts.on("--memory-model-debug", "Memory model debugging instrumentation") do |m|
      @smack_opts << "-mem-mod-dbg"
    end

    opts.on("--[no-]memory-model-impls", "Use implementations of memory procedures") do |m|
      @smack_opts.delete("-mem-mod-impls") unless m
    end

    opts.on("--[no-]memory-model-asserts", "Enable memory model assertions") do |m|
      @smack_opts << "-no-mem-mod-asserts" unless m
    end      
  end
  
  def clang
    err "cannot find 'clang' in executable path." if `which clang`.empty?
    return "clang"
  end

  def llvmlink
    err "cannot find LLVM's 'llvm-link' in executable path." if `which llvm-link`.empty?
    return "llvm-link"
  end

  def llvmopt
    err "cannot find LLVM's 'opt' in executable path." if `which opt`.empty?
    return "opt"
  end

  def smacklib
    # search the (Ruby) library path for the smack dynamic library
    $:.each do |path|
      ["smack.dylib", "smack.so", "smack.dll"].each do |lib|
        return File.join(path,lib) if File.exists?(File.join(path,lib))
      end
    end
    err "cannot find smack.{dylib,so,dll} in library path."
  end

  def smack
    return "#{llvmopt()} -load #{smacklib()} " \
    "-internalize --mem2reg -die -lowerswitch -bpl_print -debug-only=bpl -debug -disable-output"
  end

  def translate(sources)
    llvmbcs = []
    bpl = @outfile \
      || sources.map{|f| File.basename(f,File.extname(f))} * "+" + ".bpl"

    # 1. compile the sources one by one to LLVM bytecode
    sources.each do |src|
      llvmbcs << bc = "#{File.basename(src,File.extname(src))}.bc"
      puts "* Clang: #{src} => #{bc.light_blue}" unless @quiet
      cmd = "#{clang()} #{@clang_opts * " "} -g -c #{src} -emit-llvm -o #{bc}"
      puts cmd if @verbose
      err "failed to compile #{src}." unless system(cmd)
    end

    # 2. link the bytecodes together
    bc = File.basename(bpl,'.bpl') + '.bc'
    if llvmbcs.length > 1 then
      puts "* link: #{llvmbcs * ", "} => #{bc.light_blue}" unless @quiet
      cmd = "#{llvmlink()} -o #{bc} #{llvmbcs * " "}"
      puts cmd if @verbose
      err "failed to link #{llvmbcs * ", "}." unless system(cmd)
      llvmbcs << bc
    end

    # 3. translate the bytecode to Boogie
    puts "* SMACK: #{bc} => #{bpl.blue}" unless @quiet
    cmd = "#{smack()} #{@smack_opts * " "} #{bc} 2> #{bpl}"
    puts cmd if @verbose
    err "failed to translate bytecode to Boogie." unless system(cmd)

    # 4. remove temporary files
    File.delete( *llvmbcs ) unless @keep

    return bpl
  end

end

if __FILE__ == $0 then
  include Tool
  include Clang2Bpl
  version 1.0
  
  run do 
    err "Must specify some source files." unless ARGV.size > 0
    ARGV.each do |src|
      err "Source file '#{src}' does not exist." unless File.exists?(src)
    end
    t0 = Time.now()
    translate(ARGV)
    puts "#{File.basename $0} finished in #{(Time.now() - t0).round(2)}s." unless @quiet
  end
end
