#!/usr/bin/env ruby

MYVERSION = "0.1"
C2S = "#{File.dirname $0}/c2s"
BOOGIE = "Boogie"
CLEANUP = false
$graph = false

puts "FiFoSeq version #{MYVERSION}"

def usage()
    puts "usage: fifoseq.rb <impl>.bpl <K>"
end

def check_file(file,kind,ext)
    if not file \
    or not File.exists? file \
    or not File.extname(file) == ".#{ext}"
    then
        puts "Please give a #{kind} source file -- .#{ext} extension."
        usage()
        exit
    end
end

def bpl_source(file)
    check_file(file,"Boogie source","bpl")
    return file
end

def escape(filename)
  "\"#{filename}\""
end

def prepare()
  sources, rest = ARGV.partition{|f| File.extname(f) == ".bpl"}
  phases, rest = rest.partition{|a| a =~ /\/phaseBound:[0-9]+/}
  delays, rest = rest.partition{|a| a =~ /\/delayBound:[0-9]+/}
  m2s, rest = rest.partition{|a| a =~ /\/multitosingle/}
  graph, rest = rest.partition{|a| a =~ /\/graph(Of)?Trace/}

  if sources.empty? then
  	puts "Please specify at least one Boogie source file (.bpl)."
  	exit -1
  end
  
  sources.each do |f|
    if not File.exists?(f) then
      puts "Cannot find file `#{f}'."
      exit -1
    end
  end

  if phases.empty? then
  	puts "Using default /phaseBound:1."
  	phases = 1
  else
    phases = phases.first.sub(/\/phaseBound:([0-9]+)/, '\1').to_i
  end

  if delays.empty? then
  	puts "Using default /delayBound:0."
    delays = 0
  else
    delays = delays.first.sub(/\/delayBound:([0-9]+)/, '\1').to_i
  end

  m2s = !m2s.empty?  
  $graph = !graph.empty?
  rest = rest * " "

  src = "#{File.basename(sources.last,'.bpl')}.comp.bpl"
  puts "Combining [#{sources * ", "}] into #{src}." if sources.length > 1
  `cat #{sources.map{|s| escape(s)} * " "} > #{src}`
  
  puts " #{"-"*78} "
  return src, m2s, phases, delays, rest
end 

def sequentialize( src, m2s, phases, delays )
    
  puts "Sequentializing #{src} with #{phases}-phase translation."
  puts "-- Phases: #{phases}"
  puts "-- Delays: #{delays}"
  
  seq = "#{File.basename(src,'.bpl')}.FiFoSeq.#{phases}.#{delays}.bpl"

  cmd = [
    C2S, src,
    "--seq-framework",
    m2s ? "--multi-to-single" : "",
    "--phase-bounding #{phases} #{delays}",
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
      File.delete("#{src}.trace") if CLEANUP
    else
      puts output
    end
    puts "Finished in #{Time.now - t0}s."
  end 
  
  puts " #{"-" * 78} "
end

def cleanup( files )
  File.delete( *files ) if CLEANUP
end

src, m2s, phases, delays, rest = prepare()
seq = sequentialize( src, m2s, phases, delays )
verify( seq, rest )
cleanup( [src, seq] )
