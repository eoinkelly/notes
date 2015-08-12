end_app = lambda do |env|
  p env
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
end
