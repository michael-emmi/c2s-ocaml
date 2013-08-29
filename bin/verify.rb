#!/usr/bin/env ruby

require 'colorize'
require_relative 'prelude'

def boogie()
  ['Boogie','boogie','Boogie.exe','boogie.exe'].each do |b|
    return "#{b}" if not `which #{b}`.empty?
  end
  err "cannot find 'Boogie' in executable path."
end

def verify(src, options)
  case options.verifier
  when "Boogie-SI"
    verify_with_boogie_si(src,options)
  when "Boogie-FI"
    verify_with_boogie_fi(src,options)
  else
    err "invalid back-end."
  end
end

def verify_with_boogie_si(src, options)
  puts "* Boogie: #{src}" unless options.quiet
  warn "without specifying a /recursionBound, Boogie might not terminate." \
    unless options.boogie.index{|o| o =~ /\/recursionBound/}
  cmd = "#{boogie()} #{src} /stratifiedInline:2 /extractLoops /errorLimit:1 /errorTrace:2 #{options.boogie * " "}"
  puts cmd if options.verbose
  t = Time.now
  output = `#{cmd}`
  cleanup = []
  if not $?.success? then
    err "problem with Boogie: #{output}"
  else
    if options.graph && output =~ /[1-9][0-9]* errors?/ then
      File.open("#{src}.trace",'w'){|f| f.write(output) }
      `boogie-trace-parser.rb #{src}.trace`
      cleanup << "#{src}.trace"
    else
      if options.quiet then
        puts output.lines.select{|l| l =~ /[0-9]* verified/}[0]
      else
        puts output.lines.reject{|l| l.strip.empty?} * ""
      end
    end
    puts "Boogie finished in #{Time.now - t}s." unless options.quiet
  end 
  File.delete( *cleanup ) unless options.keep
end

def verify_with_boogie_fi(src, options)
  puts "* Boogie: #{src}" unless options.quiet
  warn "without specifying a /loopUnroll, Boogie might be imprecise." \
    unless options.boogie.index{|o| o =~ /\/loopUnroll/}
  cmd = "#{boogie()} #{src} /errorLimit:1 /errorTrace:2 #{options.boogie * " "}"
  puts cmd if options.verbose
  t = Time.now
  output = `#{cmd}`
  cleanup = []
  if not $?.success? then
    err "problem with Boogie: #{output}"
  else
    if options.graph && output =~ /[1-9][0-9]* errors?/ then
      File.open("#{src}.trace",'w'){|f| f.write(output) }
      `boogie-trace-parser.rb #{src}.trace`
      cleanup << "#{src}.trace"
    else
      if options.quiet then
        puts output.lines.select{|l| l =~ /[0-9]* verified/}[0]
      else
        puts output.lines.reject{|l| l.strip.empty?} * ""
      end
    end
    puts "Boogie finished in #{Time.now - t}s." unless options.quiet
  end 
  File.delete( *cleanup ) unless options.keep
end