module C2S

  require 'scriptprelude'
  
  BOOGIE = "Boogie"
  
  def self.uniquify(f)
    if true then return f end
    ext = File.extname(f)
    base = File.basename(f,ext)    
    i = 0
    while File.exists? f do
      i = i + 1
      f = base + "-#{i}" + ext
    end
    return f
  end
  private_class_method :uniquify
  
  def self.clang_frontend( clangsources, clangflags = [], bplsources = [], cleanup = true )      
    
    raise "Don't know how to handle both Clang and Boogie sources at the same time." \
      unless clangsources.empty? or bplsources.empty?
  
    sources = bplsources + clangsources
    
    src = uniquify( sources.map{|f| File.basename(f,File.extname(f))} * "+" + ".bpl" )
    puts "Compiling #{sources * ", "} into #{src}.".underline
    t0 = Time.now
    if not bplsources.empty? then
      system("cat #{sources.map{|s| s.escape} * " "} > #{src}")
    
    elsif not system("clang2bpl.rb #{clangflags * " "} #{sources * " "} -o #{src}\
      #{if not cleanup then "--keep-files" end}") then
      err "failed to translate Clang to Boogie."
      exit -1
    end
    puts "Finished in #{Time.now - t0}s".red
    puts " #{"-"*78} "
    return src
  end
  
  def self.delaybounding( src, rounds, delays )
    puts "Sequentializing #{src} with #{delays}-delay translation.".underline
    puts "* Rounds: #{rounds}"
    puts "* Delays: #{delays}"
    
    seq = "#{File.basename(src,'.bpl')}.EQR.#{rounds}.#{delays}.bpl"
    puts "* c2s: #{src} => #{seq}"
    sequentialize( src, "--delay-bounding #{rounds} #{delays}", seq )
  end
  
  def self.phasebounding( src, phases, delays, mutli = false )
    puts "Sequentializing #{src} with #{phases}-phase translation."
    puts "-- Phases: #{phases}"
    puts "-- Delays: #{delays}"
  
    seq = "#{File.basename(src,'.bpl')}.FiFoSeq.#{phases}.#{delays}.bpl"
    puts "* c2s: #{src} => #{seq}"
    sequentialize( src, "--phase-bounding #{rounds} #{delays}", seq )
    
    # TODO : pass --mutli-to-single if multi = true
  end
  
  def self.sequentialize( src, mode, seq )
    cmd = [
      "c2s", src,
      "--seq-framework",
      mode,
      "--prepare-for-back-end",
      "--print #{seq}"
    ]

    t0 = Time.now
    output = `#{cmd * " "}`
    if not $?.success? then
      puts "Sequentialization failed:"
      puts "#{cmd * " "}"
      puts output
      exit
    else
      puts output
      puts "Finished in #{Time.now - t0}s".red
    end

    puts " #{"-" * 78} "
    return seq
  end
  private_class_method :sequentialize

  def self.verify( src, args, cleanup = true, doGraph = false )  
    puts "Verifying #{src} with Boogie/Corral...".underline
    puts "* /stratifiedInline:2"
    puts "* /extractLoops"
    puts "* /errorLimit:1"
    puts "* /errorTrace:2"
    puts "* and: #{args * ", "}" if not args.empty?

    cmd = [ 
      BOOGIE,
      src, 
      "/stratifiedInline:2", "/extractLoops", 
      args, 
      "/errorLimit:1", "/errorTrace:2"
    ]
    
    if not args.any? {|a| a =~ /\/recursionBound:[0-9]+/} then
      warn "without specifying a /recursionBound:_, this may not terminate!"
    end

    # other interesting flags: 
    # /errorLimit:1 -- only one error (per procedure)
    # /errorTrace:2 -- include all trace labels in error output

    t0 = Time.now
    output = `#{cmd * " "}`
    if not $?.success? then
      puts "Verification failed:"
      puts "#{cmd * " "}"
      puts output
      exit
    else
      if doGraph && output =~ /[1-9][0-9]* errors?/ then
        File.open("#{src}.trace",'w'){|f| f.write(output) }
        `boogie-trace-parser.rb #{src}.trace`
        File.delete("#{src}.trace") if cleanup
        puts "Generated trace graph.".cyan
      else
        puts output.cyan
      end
      puts "Finished in #{Time.now - t0}s.".red
    end 
  
    puts " #{"-" * 78} "
  end
  
end
