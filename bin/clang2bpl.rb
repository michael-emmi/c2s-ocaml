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

  def smack
    err "cannot find 'smack' in executable path." if `which smack`.empty?
    return "smack"
  end

  def translate(sources)
    llvmbcs = []
    bpl = @outfile \
      || sources.map{|f| File.basename(f,File.extname(f))} * "+" + ".bpl"

    # 1. compile the sources one by one to LLVM bytecode
    sources.each do |src|
      tempfile( bc = "#{File.basename(src,File.extname(src))}.bc" )
      puts "* Clang: #{src} => #{bc.light_blue}" unless @quiet
      cmd = "#{clang()} #{@clang_opts * " "} -g -c #{src} -emit-llvm -o #{bc}"
      puts cmd.bold if @verbose
      err "failed to compile #{src}." unless system(cmd)
    end

    # 2. link the bytecodes together
    bc = File.basename(bpl,'.bpl') + '.bc'
    if llvmbcs.length > 1 then
      puts "* link: #{llvmbcs * ", "} => #{bc.light_blue}" unless @quiet
      cmd = "#{llvmlink()} -o #{bc} #{llvmbcs * " "}"
      puts cmd.bold if @verbose
      err "failed to link #{llvmbcs * ", "}." unless system(cmd)
      tempfile(bc)
    end

    # 3. translate the bytecode to Boogie
    puts "* SMACK: #{bc} => #{bpl.blue}" unless @quiet
    cmd = "#{smack()} #{@smack_opts * " "} #{bc} -o #{bpl}"
    puts cmd.bold if @verbose
    err "failed to translate bytecode to Boogie." unless system(cmd)

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
    translate(ARGV)
  end
end
