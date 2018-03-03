#!/usr/bin/env ruby

puts "Running rubocop:"
system "bundle exec rubocop -D"

puts "Running scss-lint:"
system "scss-lint"

puts "Running npm test:"
system "npm test"

begin
  require "net/http"
  require "json"

  CODESHIP_API_KEY = ENV["CODESHIP_API_KEY"].freeze

  origin_remote_url = `git config --get remote.$(git config --get branch.master.remote).url`.chomp
  current_project_name = %r{(?:\:|\/)(.+)\.git$}.match(origin_remote_url)[1]
  current_branch_name = `git rev-parse --abbrev-ref HEAD`.chomp
  codeship_projects_url = "https://codeship.com/api/v1/projects.json?api_key=#{CODESHIP_API_KEY}"

  projects_json = Net::HTTP.get(URI(codeship_projects_url))
  projects = JSON.parse(projects_json)["projects"]
  project = projects.find { |p| p["repository_name"] == current_project_name }
  exit unless project

  codeship_project_uuid = project["uuid"]
  codeship_project_id = project["id"]

  codeship_badge_md = "[ ![Codeship Status for #{current_project_name}](https://codeship.com/projects/#{codeship_project_uuid}/status?branch=#{current_branch_name})](https://codeship.com/projects/#{codeship_project_id})" # rubocop:disable Metrics/LineLength

  puts <<-EOM
Codeship build badge:
#{codeship_badge_md}
EOM
rescue StandardError => e
  puts "Failed to create a Codeship badge because #{e}"
end
