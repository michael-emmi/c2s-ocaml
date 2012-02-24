#!/usr/bin/ruby

require 'uri'
require 'net/http'
require 'CGI'

# Just ensure that "windows" points to the correct ip address of the z3 web
# server, which can be set statically on VMWare in the dhcpd.conf
# configuration file.  Check out the following article:
# http://superuser.com/questions/72199/address-vmware-fusion-linux-guest-by-hostname
# To summarize:
# 1. Shut down the VM, then open the Network Adapter Settings, click on
# Advanced Options, generate a MAC Address, and take note of it.
# 2. Open /Library/Application Support/VMware Fusion/vmnet8/dhcpd.conf and add
# the entry:
#   host windows {
#       hardware ethernet 00:50:56:21:cc:f8;
#       fixed-address 172.16.62.187;
#       option broadcast-address 172.16.62.255;
#       option domain-name-servers 172.16.62.2;
#       option domain-name localdomain;
#       option routers 172.16.62.2;
#   }
# ... adjusting the values depending on your MAC address and actual network
# addresses.
# 3. Shut down VMware completely, then run (as administrator)
#     /Library/Application Support/VMware Fusion/boot.sh --restart
# 4. Now add the following line to /etc/hosts
#    172.16.62.187 windows
# 5. Voila.  Now start up the virtual machine, ensure the IP address is set
# correctly, and from now on refer to the machine as "windows".
host = "windows"

url = "http://#{host}/BoogieServer.php"
kurl = "http://#{host}/KillBoogie.php"
version = []
flags = []
files = []
input = ""
http = []
mytimeout = 60

# if ARGV.length < 1
#   puts "Usage BoogieClient input_file.bpl"
#   exit
# end

# sort the arguments into files and flags
ARGV.each { |arg|
  if arg.include?('.bpl')
    files.push(arg)
  elsif arg.include?('/timeout:')
    mytimeout = arg.split(':')[1].to_i
  else
    flags.push(arg)
  end
}

# concatenate the contents of all input files
files.each { |f| 
  File.open(f, 'r').each_line { 
    |l| input = input + l 
  }
}

# read any possible input from standard input
if ! $stdin.isatty
  $stdin.readlines.each { |l|
    input = input + l + "\n"
  }
end

# send the HTTP request, and read the response
begin
  started = Time.now
  if (mytimeout > 0) then
    uri = URI.parse(url);
    http = Net::HTTP.start( uri.host, uri.port );
    http.read_timeout = mytimeout;
    req = Net::HTTP::Post.new(uri.path);
    req.set_form_data({ 'data'=>input, 'options'=>flags.join(' ') });
    res = http.request( req );
    puts res.body.chomp;
    http.finish
  else
    response =
    Net::HTTP.post_form( URI.parse(url),
                          { 'version'=>version,
                            'data'=>input,
                            'options'=>flags.join(' ')});
    puts response.body.chomp
end
rescue Timeout::Error => e
  puts "Timeout: #{e}."
  ended = Time.now
  puts "Time: #{ended - started}"
rescue Interrupt => e
  puts "Interrupted: #{e}."
ensure
  Net::HTTP.get( URI.parse(kurl));
end

