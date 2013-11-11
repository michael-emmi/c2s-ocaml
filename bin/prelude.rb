#!/usr/bin/env ruby

require 'eventmachine'
require 'optparse'
require 'colorize'
require 'curses'
require 'set'

def err( msg )
  puts "Error: #{msg}".red
  exit -1
end

def warn( msg )
  @warned ||= Set.new
  return if @warned.include? msg
  puts "Warning: #{msg}".yellow
  @warned.add msg
end

module Tool
  
  attr_accessor :quiet, :verbose, :keep, :banner, :tempfiles, :extra_args
    
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
      opts.banner = "Usage: #{File.basename $0} [options] FILE(s)"
      self.class.included_modules.reverse.concat([self.class]).each do |m|
        if m.instance_methods(false).include?(:options) then
          m.instance_method(:options).bind(self).call(opts)
        end
      end
    end.parse!
    
    t0 = Time.now()
    yield if block_given?
    puts "#{File.basename $0} finished in #{(Time.now() - t0).round(2)}s." unless @quiet
    File.delete( *@tempfiles ) unless @keep
  end
  
  def version(v)
    @version = v
  end  
  
  def tempfile(f)
    @tempfiles = [] unless @tempfiles
    @tempfiles << f
    f
  end
  
  def task(desc = "This")
    t = Time.now
    res = nil
    EventMachine.run do
      EventMachine.defer do
        res = yield if block_given?
        EventMachine.stop
        puts
      end
      EventMachine.add_periodic_timer( 1 ) do
        print "#{desc} has been running for #{(Time.now - t).round} seconds so far...\r".red
      end
    end
    return res
  end    
end
