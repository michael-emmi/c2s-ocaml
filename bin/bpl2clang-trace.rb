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
    
    missing = []
    
    found = false
    
    if not File.exists?(filename) then
      puts line
      if not missing.include?(filename) then
        missing << filename
        puts "#{spaces} * Cannot locate #{filename}."
      end
      next
    end
    
    File.readlines(filename).drop(lineno-1).take(10).each do |line|
      if m = line.match(/\{:sourceloc (#{FILENAME}), (\d+), (\d+)\}/) then
        filename = m[1]
        lineno = m[2].to_i
        colno = m[3].to_i

        # TODO show some code??
        # code = File.readlines(filename)[lineno-1,5]

        puts "#{spaces}#{filename}(#{lineno},#{colno})"
        found = true
        
        break
      end
    end
    
    if not found then
      puts "#{line.partition(":")[0]}: missing {:sourceloc} annotation."
      
    end
    
  elsif m = line.match(/value = ([^ ]+)/)
    # TODO find out which BPL line it's on, then use {:sourceloc} to find the
    # original source code line containing __XXX_record_int(<expr>), then print
    # <filename>(<line>,<col>): <expr>
    
  else
    puts line
  end
end