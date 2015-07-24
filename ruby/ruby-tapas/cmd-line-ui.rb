# Minimal command prompt UI in ruby

PROMPT = '> '

while (print PROMPT; input = gets) do
  input.chomp!.downcase! # normalise input
  puts "Input: #{input.inspect}"
  case input
  when 'time' then
    puts Time.now
  when 'exit', 'quit' then
    break # exit the infinite loop
  else
    puts 'Unrecognised command'
  end
end
