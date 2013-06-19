#!/usr/bin/env ruby

require 'colorize'

$MYNAME='clang2bpl'
$MYVERSION=0.2

$clang = "clang"
$ccargs = "-g"
$llink = "llvm-link"
$llopt="opt"
$boogie="Boogie"

# $dsalib="/isd/users/zrakamar/llvm-2.5/obj/projects/poolalloc/Debug/
#   lib/libLLVMDataStructure.so"

$smackopts = [
  "-internalize", "-mem2reg", "-die", "-lowerswitch",
  "-bpl_print", 
  "-debug-only=bpl", 
  "-debug",
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

def depends( dep, kind = :exe, name = nil )
  if kind == :exe and `which #{dep}`.empty? then
    err "cannot locate `#{dep}'; make sure `#{dep}' is in your PATH."
    exit -1
    
  elsif kind == :env and not ENV[dep] then
    err "environment variable dependency `#{dep}' is undefined."
    exit -1
    
  elsif kind == :file and not File.exists?(dep) then
    err "file dependency `#{dep}' does not exist."
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
    $smackprelude = "#{$smackprefix}/include/prelude_bits_monolithic.bpl"
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
depends('SMACKPREFIX',:env)
$smackprefix = ENV['SMACKPREFIX']
depends("#{$smackprefix}/lib/smack.dylib",:file)
depends("#{$smackprefix}/include/prelude-int.bpl",:file)
$smacklib="#{$smackprefix}/lib/smack.dylib"
$smackprelude="#{$smackprefix}/include/prelude-int.bpl"
depends($clang)
depends($llink)
depends($llopt)
$smack = "#{$llopt} -load #{$smacklib}"

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
  if not system("#{$clang} #{rest * " "} -c #{src} #{$ccargs} -emit-llvm -o #{bc}") then
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

if not system("#{$smack} #{$smackopts * " "} #{bc} 2>> #{bpl}") then
  err "failed to translate bytecode to Boogie."
  exit -1
end

# remove temporary files
cleanup( llvmbcs + [ bc ] )

puts "done."
