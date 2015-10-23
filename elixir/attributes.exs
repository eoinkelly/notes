# Attributes
# ##########

# * are the equivalent of class constants in ruby
# * Their values can be changed
#     * allows for @doc to be used to document methods
# * are metadata
# * exist only at the module level (cannot be declared within functions)

defmodule Foo do
  @author "Eoin Kelly"
  @version "1.2.3"

  @doc """
  some docs
  about this method
  """
  def show_metadata_constants do
    IO.puts "#{@author} at #{@version}"
  end

  # you can redefine attributes - a good example of this is using @doc to
  # document functions
  @author "Totoro"
  @version "blah"

  @doc """
  some docs
  about another method
  """
  def show_changed_metadata_constants do
    IO.puts "#{@author} at #{@version}"
  end
end

Foo.show_metadata_constants
Foo.show_changed_metadata_constants
