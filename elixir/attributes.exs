# Attributes
# ##########

# * are the equivalent of class constants in ruby
# * Their values can be changes - why???
# * are metadata
# * exist only at the module level (cannot be declared within functions)

defmodule Foo do
  @author "Eoin Kelly"
  @version "1.2.3"

  def show_metadata_constants do
    IO.puts "#{@author} at #{@version}"
  end

  # you can redefine attributes - what is the use-case for this?
  @author "Totoro"
  @version "blah"

  def show_changed_metadata_constants do
    IO.puts "#{@author} at #{@version}"
  end
end

Foo.show_metadata_constants
Foo.show_changed_metadata_constants
