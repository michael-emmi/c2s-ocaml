#!/usr/bin/env ruby

MYVERSION = "0.1"

C2S = "c2s"
BOOGIE = "Boogie"
$cleanup = true
$graph = false

puts "Delay-bounded sequentialization version #{MYVERSION}"

def usage()
    puts "usage: dbseq.rb <impl>.bpl /rounds:_ /delayBound:_"
end

def check_file(file,kind,ext)
    if not file \
    or not File.exists? file \
    or not File.extname(file) == ".#{ext}"
    then
        puts "Please give a #{kind} source file -- .#{ext} extension."
        usage()
        exit -1
    end
end

def bpl_source(file)
    check_file(file,"Boogie source","bpl")
    return file
end

def cfg_source(file)
    check_file(file,"context-free grammar","cfg")
    return file
end

def escape(filename)
  "\"#{filename}\""
end

def prepare()
  sources, rest = ARGV.partition{|f| File.extname(f) == ".bpl"}
  rounds, rest = rest.partition{|a| a =~ /\/rounds:[0-9]+/}
  delays, rest = rest.partition{|a| a =~ /\/delayBound:[0-9]+/}
  m2s, rest = rest.partition{|a| a =~ /\/multitosingle/}
  keep, rest = rest.partition{|a| a =~ /\/keepFiles/}
  graph, rest = rest.partition{|a| a =~ /\/graph(Of)?Trace/}

  if sources.empty? then
  	puts "Please specify at least one Boogie source file (.bpl)."
    usage()
  	exit -1
  end
  
  sources.each do |f|
    if not File.exists?(f) then
      puts "Cannot find file `#{f}'."
      usage()
      exit -1
    end
  end
  
  if delays.empty? then
    puts "Using default /delayBound:0."
    delays = 0
  else 
    delays = delays.first.sub(/\/delayBound:([0-9]+)/, '\1').to_i
  end

  if rounds.empty? then
  	puts "Using default /rounds:#{delays+1}, for /delayBound:#{delays}."
    rounds = delays + 1
  else
    rounds = rounds.first.sub(/\/rounds:([0-9]+)/, '\1').to_i
  end

  m2s = !m2s.empty?
  $cleanup = keep.empty?
  $graph = !graph.empty?
  rest = rest * " "

  src = "#{File.basename(sources.last,'.bpl')}.comp.bpl"
  puts "Combining [#{sources * ", "}] into #{src}." if sources.length > 1
  `cat #{sources.map{|s| escape(s)} * " "} > #{src}`
  
  puts " #{"-"*78} "
  return src, rounds, delays, rest
end 

def sequentialize( src, rounds, delays )
  puts "Sequentializing #{src} with #{delays}-delay translation."
  puts "-- Rounds: #{rounds}"
  puts "-- Delays: #{delays}"
  
  seq = "#{File.basename(src,'.bpl')}.EQR.#{rounds}.#{delays}.bpl"

  cmd = [
    C2S, src,
    "--seq-framework",
    "--delay-bounding #{rounds} #{delays}",
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
    puts "Finished in #{Time.now - t0}s"
  end

  puts " #{"-" * 78} "
  return seq
end

def verify( src, args )  
  puts "Verifying #{src} with Boogie..."
  puts "-- /stratifiedInline:1"
  puts "-- /extractLoops"
  puts "-- /errorLimit:1"
  puts "-- /errorTrace:2"
  puts "-- and: #{args}" if not args.empty?

  cmd = [ 
    BOOGIE, src, 
    "/stratifiedInline:1", "/extractLoops", 
    args, 
    "/errorLimit:1", "/errorTrace:2"
  ]

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
    if $graph && output =~ /[1-9][0-9]* errors?/ then
      File.open("#{src}.trace",'w'){|f| f.write(output) }
      `boogie-trace-parser.rb #{src}.trace`
      File.delete("#{src}.trace") if $cleanup
    else
      puts output
    end
    puts "Finished in #{Time.now - t0}s."
  end 
  
  puts " #{"-" * 78} "
end

def cleanup( files )
  File.delete( *files ) if $cleanup
end

src, rounds, delays, rest = prepare()
seq = sequentialize( src, rounds, delays )
verify( seq, rest )
cleanup( [src, seq] )
