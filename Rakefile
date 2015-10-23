require 'rake'
require 'pry'

markdowns = Rake::FileList.new('**/*.md') do |fl|
  fl.exclude do |f|
    `git ls-files #{f}`.empty?
  end
end

# all foo/bar/blah.md becomes foo/bar/blah.html
htmls = markdowns.ext('html').pathmap('dist/%p')

task default: :html

task html: htmls

# tell rake to create the dist dir if it does not exist
# also creates a "directory task"
directory 'dist'

# tell rake how to convert a markdown file to html
rule '.html' => ['.md', 'dist'] do |t| # t is instance of Rake::Task
  # puts "mkdir -p #{t.name.pathmap('%d')}"
  # puts "pandoc -o #{t.name} #{t.source}"
  mkdir_p t.name.pathmap('%d')
  sh "pandoc -o dist/#{t.name} #{t.source}"
end

task :clean do
  rm_rf 'dist'
end
