#!/usr/bin/env ruby

require 'colorize'

def err( msg )
  puts "Error: #{msg}".red
  exit -1
end

def warn( msg )
  puts "Warning: #{msg}".yellow
end

def standard_options(opts, options)
  opts.on("-h", "--help", "Show this message") do
    puts opts
    exit
  end
  
  opts.on("--version", "Show version") do
    puts "#{File.basename $0} version #{$MYVERSION}"
    exit
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
end
