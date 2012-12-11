require 'colorize'

def warn( msg )
  puts "Warning: #{msg}".yellow
end

def err( msg )
  puts "Error: #{msg}".red
end

def check_file(file,kind,ext)
    if not file \
    or not File.exists? file \
    or not File.extname(file) == ".#{ext}"
    then
        err "Please give a #{kind} source file -- .#{ext} extension."
        usage()
        exit
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

class String
  def escape()
    "\"#{self}\""
  end
end

