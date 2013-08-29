#!/usr/bin/env ruby

require 'colorize'

def err( msg )
  puts "Error: #{msg}".red
  exit -1
end

def warn( msg )
  puts "Warning: #{msg}".yellow
end