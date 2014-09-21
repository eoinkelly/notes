def run_lexer(data)
  data = data.unpack("c*") if(data.is_a?(String))
  eof = data.length
  token_array = []

  %% write init;
  %% write exec;

  puts token_array.inspect
end
