#!/usr/bin/env ruby

MYVERSION = "0.1"

FILENAME = /[A-Za-z0-9#$_\-~%.\/]+/
LABEL = /[A-Za-z0-9$_]+/

ARGF.each_line do |line|
  if m = line.match(/([ ]+)(#{FILENAME})\((\d+),(\d+)\): (#{LABEL})/) then
    spaces = m[1]
    filename = m[2]
    lineno = m[3].to_i
    colno = m[4].to_i
    label = m[5]
    
    found = false
    
    File.readlines(filename).drop(lineno-1).take(10).each do |line|
      if m = line.match(/\{:sourceloc (#{FILENAME}), (\d+), (\d+)\}/) then
        filename = m[1]
        lineno = m[2].to_i
        colno = m[3].to_i
        
        # code = File.readlines(filename)[lineno-1,5]

        puts "#{spaces}#{filename}(#{lineno},#{colno})"
        found = true
        
        break
      end
    end
    
    puts line if not found
    
  else
    puts line
  end
end