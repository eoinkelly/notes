# lexer.rl
%%{
  machine test_lexer;
  integer = ('+'|'-')?[0-9]+;

  main := |*
    integer => {
      # 'ts' stands for 'token start', while 'te' stands for 'token end'. These
      # represent the indices from our data array that match the start and end
      # of the current token.
      puts "Integer: " + data[ts..te].pack("c*")
    };
  *|;
}%%

%% write data;
