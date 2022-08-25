# frozen_string_literal: true

# Some attempts to make a FizzBuzz that generates data as fast as possible

def attempt_1
  (1...1_000_000).each do |n|
  # (1..).each do |n|
    if n % 3 == 0 && n % 5 == 0
      puts "FizzBuzz"
      next
    end

    if n % 3 == 0
      puts "Fizz"
      next
    end

    if n % 5 == 0
      puts "Buzz"
      next
    end

    puts n
  end
end

def attempt_2
  (1..).each do |n|
    if (n % 3).zero? && (n % 5).zero?
      puts "FizzBuzz"
      next
    end

    if (n % 3).zero?
      puts "Fizz"
      next
    end

    if (n % 5).zero?
      puts "Buzz"
      next
    end

    puts n
  end
end

attempt_1
