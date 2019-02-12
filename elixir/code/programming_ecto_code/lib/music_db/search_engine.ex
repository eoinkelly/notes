#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule SearchEngine do
  def update!(item) do
    # search engine logic happens here...
    {:ok, item}
  end

  def update(item) do
    # search engine logic happens here...
    {:ok, item}
  end

  def update(_repo, changes, extra_argument) do
    # search engine logic happens here...
    {:ok, {changes, extra_argument}}
  end
end
