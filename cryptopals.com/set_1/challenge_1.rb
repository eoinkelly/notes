require "rspec"
require "base64"
require "pry"

def hex_to_bin(s)
  s.scan(/../).map { |x| x.hex.chr }.join
end

def bin_to_hex(s)
  s.each_byte.map { |b| b.to_s(16) }.join
end

def xor_hex(hex_1, hex_2)
  result_bin = xor_bin(hex_to_bin(hex_1), hex_to_bin(hex_2))
  bin_to_hex(result_bin)
end

def xor_bin(bin_1, bin_2)
  bin_1.bytes.zip(bin_2.bytes)
    .map { |a, b| a ^ b }
    .map(&:chr).join
end

##
# A (very) rough test to decide whether a string contains an human readable
# message
#
def for_humans?(str)
  num_printables = str.scan(/[[:print:]]/).length
  num_vowels = str.scan(/[aeiou]/).length
  num_asciis = str.scan(/[[:ascii:]]/).length
  num_punct = str.scan(/[[:punct:]]/).length

  num_asciis == str.length &&                # 100% ascii chars
    num_punct <= (str.length * 0.05) &&      # at most 10% punctuation
    num_printables >= (str.length * 0.98) && # at least 98% printable chars
    num_vowels >= (str.length * 0.1)         # at least 10% vowels
end

def for_humans_2?(str)
  num_printables = str.scan(/[[:print:]]/).length
  num_vowels = str.scan(/[aeiou]/).length
  num_asciis = str.scan(/[[:ascii:]]/).length
  num_punct = str.scan(/[[:punct:]]/).length

  num_asciis == str.length # &&                # 100% ascii chars
    # num_punct <= (str.length * 0.05) &&      # at most 10% punctuation
    # num_printables >= (str.length * 0.98) && # at least 98% printable chars
    # num_vowels >= (str.length * 0.1)         # at least 10% vowels
end

RSpec.describe "Challenge set 1" do
  it "Converting hex string to base64" do
    input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    expected_output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

    binary_input = hex_to_bin(input)
    actual_output = Base64.strict_encode64(binary_input)

    expect(actual_output).to eq expected_output
  end

  it "Fixed XOR" do
    input_1 = "1c0111001f010100061a024b53535009181c"
    input_2 = "686974207468652062756c6c277320657965"
    expected_output = "746865206b696420646f6e277420706c6179"

    result = xor_hex(input_1, input_2)

    expect(result).to eq(expected_output)
  end

  it "single byte XOR cipher" do
    cipher_hex = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    cipher = hex_to_bin(cipher_hex)

    possibles = (0..255).map(&:chr).map do |key|
      # puts "Trying key: #{key} (binary value #{key.ord})"
      padded_key = key * cipher.length
      [key, xor_bin(cipher, padded_key)]
    end

    probables = possibles.select do |_key, str|
      for_humans?(str)
    end

    probables.each do |key, str|
      puts "Key: #{key} (binary value #{key.ord})"
      puts "Clear text: #{str}"
    end
  end

  it "Detect single-character XOR" do
    ciphers = File.readlines("challenge_4_data.txt").map(&:chomp).map { |line| hex_to_bin(line) }

    ciphers.each do |cipher|
      # FIXME: this is just a paste of the code from test above
      possibles = (0..255).map(&:chr).map do |key|
        # puts "Trying key: #{key} (binary value #{key.ord})"
        padded_key = key * cipher.length
        [key, xor_bin(cipher, padded_key)]
      end

      probables = possibles.select do |_key, str|
        for_humans_2?(str)
      end

      puts "Cipher: #{bin_to_hex(cipher)}" unless probables.empty?
      probables.each do |key, str|
        puts "Key: #{key} (binary value #{key.ord})"
        puts "Clear text: #{str}"
      end
    end
  end
end
