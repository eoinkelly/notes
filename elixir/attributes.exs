# Attributes
# ##########

# * have a similar usage to constants in ruby
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

#################################################
#################################################
#################################################
# chris mccord uses this pattern in his code - why does he assign ?

defmodule Thing do
	phoenix_path = Application.app_dir(:phoenix, "priv/static/phoenix.js")
	reload_path  = Application.app_dir(:phoenix_live_reload, "priv/static/phoenix_live_reload.js")
	@external_resource phoenix_path
	@external_resource reload_path
	@phoenix_js File.read!(phoenix_path)

	# ...
end
