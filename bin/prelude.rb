#!/usr/bin/env ruby

require 'optparse'
require 'colorize'

module Tool
  
  attr_accessor :quiet, :verbose, :keep
    
  def options(opts)
    
    @quiet = false
    @verbose = false
    @keep = false

    opts.separator ""
    opts.separator "Basic options:"
    
    opts.on("-h", "--help", "Show this message") do
      puts opts
      exit
    end
  
    opts.on("--version", "Show version") do
      puts "#{File.basename $0} version #{@version || "??"}"
      exit
    end
  
    opts.on("-v", "--[no-]verbose", "Run verbosely") do |v|
      @verbose = v
      @quiet = !v
    end

    opts.on("-q", "--[no-]quiet", "Run very quietly") do |q|
      @quiet = q
      @verbose = !q
    end

    opts.on("-k", "--[no-]keep-files", "Don't delete intermediate files") do |v|
      @keep = v
    end
  end
  
  def run
    OptionParser.new do |opts|
      self.class.included_modules.reverse.concat([self.class]).each do |m|
        if m.instance_methods(false).include?(:options) then
          m.instance_method(:options).bind(self).call(opts)
        end
      end
    end.parse!

    yield if block_given?
  end
  
  def version(v)
    @version = v
  end
  
  def err( msg )
    puts "Error: #{msg}".red
    exit -1
  end

  def warn( msg )
    puts "Warning: #{msg}".yellow
  end
  
end
