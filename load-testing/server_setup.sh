#!/bin/bash

# Steps for setting up an AWS ubuntu instance for running a rails server with
# database in RDS

# This script is designed to be run as a non-root user

sudo apt-get update -y
# sudo apt-get upgrade -y

# install basic development tools
# ###############################

# TODO: not sure I need all these
sudo apt-get install -y git-core curl zlib1g-dev build-essential libssl-dev libreadline-dev libyaml-dev libsqlite3-dev sqlite3 libxml2-dev libxslt1-dev libcurl4-openssl-dev python-software-properties libffi-dev htop libpq-dev

# Install and setup node
# ######################

curl -sL https://deb.nodesource.com/setup_6.x | sudo -E bash -
sudo apt-get install -y nodejs

# Install and setup ruby
# ######################

git clone https://github.com/rbenv/rbenv.git $HOME/.rbenv
echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> $HOME/.bash_profile
echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> $HOME/.bashrc
export PATH="$HOME/.rbenv/bin:$PATH"

echo 'eval "$(rbenv init -)"' >> $HOME/.bashrc
eval "$(rbenv init -)"

git clone https://github.com/rbenv/ruby-build.git $HOME/.rbenv/plugins/ruby-build
echo 'export PATH="$HOME/.rbenv/plugins/ruby-build/bin:$PATH"' >> $HOME/.bashrc
echo 'export PATH="$HOME/.rbenv/plugins/ruby-build/bin:$PATH"' >> $HOME/.bash_profile
export PATH="$HOME/.rbenv/plugins/ruby-build/bin:$PATH"

rbenv install 2.4.1
rbenv global 2.4.1
gem update --system --no-rdoc --no-ri
gem install --no-rdoc --no-ri bundler

# export RAILS_ENV=production
#
# # get the rails app on the server and cd into it
#
# RAILS_ENV=production bundle exec unicorn -p 3000 -c ./config/unicorn.rb -D
# # -D daemonizes, you may not want to use it when getting things going
