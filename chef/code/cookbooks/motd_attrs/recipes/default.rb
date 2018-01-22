#
# Cookbook:: motd_attrs
# Recipe:: default
#
# Copyright:: 2018, The Authors, All Rights Reserved.
#

# node.default["motd_attrs"]["special_message"] = "This is set by an attributre"

template "/etc/motd" do
  source "motd.erb"
  mode "0644"
end
