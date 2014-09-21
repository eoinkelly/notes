require 'pry'

class Factor
  def words
    %w( add product [a,b] . )
  end

  def initialize(code, stack = [])
    @code = code
    @stack = stack
    @custom_words = {}
  end

  def run
    words = @code.split(' ')

    words.each do |word|
      if evalable?(word)
        case word
        when '[a,b]'
          send :make_range
        when '.'
          send :dot
        else
          send word
        end
      else
        @stack.push word
      end
    end
  end

  def evalable?(word)
    words.include? word
  end

  # words
  # #####

  def factorial
  end

  def add
    a = @stack.pop.to_i
    b = @stack.pop.to_i
    @stack.push(a + b)
  end

  def product
    a = @stack.pop.to_i
    b = @stack.pop.to_i
    @stack.push(a * b)
  end

  def make_range
    last = @stack.pop.to_i
    first = @stack.pop.to_i
    @stack.push Range.new(first, last)
  end

  def dot
    puts @stack.pop
  end
end

# How do I impelment this directly using the Yarv stack?
#
# code = ": factorial ( n -- n! ) 1 [a,b] product ;"

require 'rspec'

describe Factor do
  describe 'words' do
    describe 'add' do
      it 'works for integers' do
        code = '1 34 add'
        stack = []
        Factor.new(code, stack).run
        expect(stack).to eq([35])
      end
    end

    describe 'product' do
      it 'works for integers' do
        code = '3 4 product'
        stack = []
        Factor.new(code, stack).run
        expect(stack).to eq([12])
      end
    end

    describe '[a,b]' do
      it 'works for integers' do
        code = '3 18 [a,b]'
        stack = []
        Factor.new(code, stack).run
        expect(stack).to eq([3..18])
      end
    end
  end
end
