# Example rake file 1
# ###################

# first version: no rake, procedural ruby script
# -- slow: will rebuild each file each time you run it
%W[ch1.md ch2.md ch3.md].each do |md_file|
  html_file = File.basename(md_file, ".md") + ".html"
  system("pandoc -o #{html_file} #{md_file}")
end

# next version: uses rake
# ++
%W[ch1.md ch2.md ch3.md].each do |md_file|
  html_file = File.basename(md_file, ".md") + ".html"
  file html_file => md_file do
    sh "pandoc -o #{html_file} #{md_file}"
  end
end

# set default task to depend on the :html task
task default: :html

# :html task depends on two *files* not other tasks
task html: %w(chap1.html chap2.html)

# create a *rule* which tells rake how to build a particular filetype
rule '.html' => '.md' do |t| # t is instance of Rake::Task
  sh "pandoc -o #{t.name} #{t.source}"
end
