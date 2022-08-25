#!/usr/bin/env ruby

# this is a handy test for how Capistrano accesses a server
require 'net/ssh'
require 'pp'

host = "somehost.example.com"
user = "some_user"

Net::SSH.start(host, user, verbose: Logger::DEBUG,  use_agent: true) do |ssh|
  pp ssh.exec!("ssh -T git@github.com")
end
