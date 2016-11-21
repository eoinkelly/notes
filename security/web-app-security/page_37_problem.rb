require "uri"

# Solution to problem from page 37 (p73 in PDF) of Web application hacker
# handbook

# scrub method as specified by the problem
def scrub(str)
  p im = str.gsub(/<script>/, "") # 1. remove <script>
  p im = im[0...50]               # 2. truncate to first 50 chars
  p im = im.gsub(/("|')/, "")     # 3. remove " and '
  p im = URI.unescape(im)         # 4. URL decode the input
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

desired_malicious_output = '"><script>alert("foo")</script>'

inputs = [
  # does not work: desired output without changes
  # "><script>alert('foo')</script>",

  # does not work: URI encode the whole thing
  # "%3E%3Cscript%3Ealert('foo')%3C/script%3E",

  # gets <script> through
  # "><%00script>alert('foo')</script>",

  # does not work:
  # ">%3Cscript%3Ealert('foo')</script>",

  # does not work: embed <script>
  # "><<script>script>alert('foo')</script>"

  # ASCII table
  # %25 = %
  # %22 = "

  # encode " somehow so it passes step 3
  # need to somehow preserve length
  # '"><%00script>alert(%2522foo%2522)</script>',

  #
  # %00 goes from 3 chars to 0 chars
  # %25 goes from 3 chars to 1 char
  #
  # I want a string that does not contain ' or "
  # but will after url decoding
  #
  # so waht ways can i create percent encodings that will become ' or "
  #
  # KNOWN: a url-decode of a character always makes the string shorter
  # => I need a way to make the scrubber make the string longer
  # The only viable way I can see to do this is via the urlencode step
  '><%00script>alert(%22foo%22)</script>',
  '%22>%3cscript>alert(%22foo%22)</script>' # does not work: proposed solution from book
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
