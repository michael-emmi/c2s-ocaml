#!/usr/bin/env ruby

require 'colorize'

$MYNAME='clang2bpl'
$MYVERSION=0.1

$clang = "clang"
$llink = "llvm-link"
$smack="opt"
$boogie="Boogie"

$smackhome="/Users/mje/Code/Tools/llvm/projects/smack"
$smackprelude="#{$smackhome}/bin/prelude-int.bpl"
$smacklib="#{$smackhome}/Debug+Asserts/lib/smack.dylib"
#$dsalib="/isd/users/zrakamar/llvm-2.5/obj/projects/poolalloc/Debug/lib/libLLVMDataStructure.so"
$smackopts = [
  "-load #{$smacklib}",
  "-internalize", "-mem2reg", "-die", "-lowerswitch",
  "-bpl_print", 
  # "-debug-only=bpl", 
  # "-debug",
  "-disable-output",
  # "-raiseallocs", 
  # "-generate_lines",
]
$smackfilter = "sed -n '/SMACK-MODULE-BEGIN/,/SMACK-MODULE-END/p'"

$cleanup = true

class Array
  def uniq
    u = []
    self.each do |e|
      if not u.include?(e) then
        u << e
      end
    end
    return u
  end
  
  def part2
    left = []
    right = []
    prev = false
    self.each do |e|
      if prev then
        prev = false
        left << e
      elsif yield e then
        prev = true
        left << e
      else
        right << e
      end
    end
    return left, right
  end
end

def version()  
  def extver(a,v,p)
    `#{a} #{v}`.match(p)[1] if not `which #{a}`.empty?
  end  
  puts "#{$MYNAME}.rb version #{$MYVERSION}"
  puts "* using #{ extver($clang, "--version", /.*(clang version [^ ]*).*/) }"
  puts "* using #{ extver($llink, "-version", /.*(LLVM version [^ ]*).*/) }"
  puts "* using #{ extver($boogie, ".", /.*(Boogie.* version [^ ]*).*/) }"
  exit 0
end

def usage()
  puts "usage: #{$MYNAME}.rb <clang-source-files> [options]"
  puts "possible options: 
  --help                display this usage message
  --version             just print the version information
  
  --keep-files          don't delete intermediate files
  --output-file <file>  specify the output file
  -o <file>             abbreviation for --output-file <file>
  
  SMACK options
  --inference           inference mode
  --monolithic          monolithic mode
  --use-dsa             DSA mode "
  exit 0
end

def depends( dep )
  if `which #{dep}`.empty? then
    err "cannot locate `#{dep}'; make sure `#{dep}' is in your PATH."
    exit -1
  end
end

def sources( rest )
  sources, rest = rest.partition{|f| File.extname(f) =~ /[.](c|cc|cpp)/}
  
  if sources.empty? then
  	err "must specify at least one Clang source file (e.g. C/C++)."
    puts "Type clang2bpl.rb --help for usage."
  	exit -1
  end
  
  sources.each do |f|
    if not File.exists?(f) then
      err "cannot locate source file `#{f}'."
      exit -1
    end
  end
  
  return sources, rest
end

def args( rest )
  help, rest = rest.partition{|a| a =~ /--help/}
  version, rest = rest.partition{|a| a =~ /--version/}
  keep, rest = rest.partition{|a| a =~ /--keep-files/}
  outfile, rest = rest.part2{|a| a =~ /--output-file|-o/}
  inference, rest = rest.partition{|a| a =~ /--inference/}
  monolithic, rest = rest.partition{|a| a =~ /--monolithic/}
  usedsa, rest = rest.partition{|a| a =~ /--use-dsa/}
  
  usage() if not help.empty?
  version() if not version.empty?
  
  $cleanup = keep.empty?
  
  if not outfile.empty? and outfile.length < 2 then
    warn "found -o/--output-file without subsequent filename."
  end
  $outfile = outfile.length > 1 ? outfile[1] : nil

  
  if not inference.empty? then
    $smackopts << "-inference"
  end

  if not monolithic.empty? then
    $smackopts << "-monolithic"
    $smackprelude = "#{$smackhome}/headers/prelude_bits_monolithic.bpl"
  end
  
  if not usedsa.empty? then
    $smackopts << "-calltarget"
    $smackopts << "-load #{$dsalib}"
    usedsa = true
  end

  
  return rest
end 

def warn( msg )
  puts "Warning: #{msg}".yellow
end

def err( msg )
  puts "Error: #{msg}".red
end

def cleanup( files )
  File.delete( *(files.uniq) ) if $cleanup
end

# Let's get to it then..
rest = args(ARGV)
depends($clang)
depends($llink)
depends($smack)
csources, rest = sources(rest)

llvmbcs = []
if $outfile then
  bpl = $outfile
else
  bpl = csources.map{|f| File.basename(f,File.extname(f))} * "+" + ".bpl"
end

# translate the sources one by one, to LLVM bytecode
csources.each do |src|
  base = File.basename(src,File.extname(src))
  llvmbcs << bc = "#{base}.bc"
  puts "* Clang: #{src} => #{bc}"
  if not system("#{$clang} #{rest * " "} -c #{src} -emit-llvm -o #{bc}") then
    err "failed to compile #{src}."
    exit -1
  end  
end

# link the bytecode
bc = File.basename(bpl,'.bpl') + '.bc'
puts "* link: #{llvmbcs * ", "} => #{bc}"
if not system("#{$llink} -o #{bc} #{llvmbcs * " "}") then
  err "failed to link #{llvmbcs * ", "}."
  exit -1
end

# SMACK it together
puts "* SMACK: #{bc} => #{bpl}"
if File.exists?($smackprelude) then
  `cat #{$smackprelude} > #{bpl}`
else
  warn "cannot locate SMACK's prelude file (at `#{$smackprelude}')."
end

if not system("#{$smack} #{$smackopts * " "} #{bc} >> #{bpl}") then
  err "failed to translate bytecode to Boogie."
  exit -1
end

# remove temporary files
cleanup( llvmbcs + [ bc ] )

puts "done."
