

require "erb"

# We pass the current context in as 'binding' so the template will lookup methods here


# This class will function as the context for the template - the template will lookup all methods and ivars on this class
class EoinView
  def initialize
    @greeting = "Yo!"
  end

  def name
    "eoin"
  end

  def give_binding
    binding
  end

  def blocky
    yield 33
  end
end

template = ERB.new(File.read("test.html.erb"))
template_context = EoinView.new.give_binding

# #src is a String of ruby code, which when we run
#
#     eval(template.src, template_context) # => the rendered template
#
# will return a string of rendered content.
puts template.src # => String

puts "=" * 80

# #result will take the given context and eval() the template with that context and return the result as a string
puts template.result(template_context)

# Output:

# #coding:UTF-8
# _erbout = +''; _erbout.<< "\n<h1>This is a heading</h1>\n\n".freeze
#
#
# ; _erbout.<<(( @greeting ).to_s); _erbout.<< " ".freeze; _erbout.<<(( name ).to_s); _erbout.<< ", this is a template.\n\n".freeze
#
# ;  blocky do |x| ; _erbout.<< "\n  <h2>x</h2>\n".freeze
#
# ;  end ; _erbout.<< "\n".freeze
# ; _erbout
# ================================================================================
#
# <h1>This is a heading</h1>
#
# Yo! eoin, this is a template.
#
#
#   <h2>x</h2>
#
