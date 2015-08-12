# minimal rack app
#
# run this by
#
# $ cd path/to/this/files/dir
# $ rackup

require 'pry'
# NOTE: does not work if you used do..end for the block!!
run lambda { |_env|
  binding.pry
  [
    '200',
    {
      'Content-type' => 'application/stuff'
    },
    [
      "this is line 1\n",
      "this is line 2\n"
    ]
  ]
}
