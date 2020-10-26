require "rspec"

# answer seems to be slope of 1.844 (manual search of the space using least squared error
class LinReg
  def initialize
    @xs = []
    @ys = []
    @slope = nil

    loadtxt("./pizza.txt", skip_rows: 1)
      .each do |pair|
        @xs << pair[0]
        @ys << pair[1]
      end

    # calc_error(guess: 1.5)
  end

  def train
    # calculate the slope given arrays of x and y values
    #
    # ? maybe sort by x value first?
    #
    # Assume: line goes through 0,0 i.e. formula is y = wx where w i slope
    #
    # Algo option 1:
    #
    # guess_slope = 0
    # results = []
    # foreach result [x, y]
    #   err = calc_square_error_for_all_points(xs, ys, guess_slope)
    #   results << { guess_slope: guess_slope, err: err }
    #
    # search the space of possible slopes
    # possible slope values center around 0 so start there
    #
    ref_guess = 0
    high_guess = 1
    low_guess = -1


    loop do
      puts "looping ..."

      ref_err = calc_error(guess: ref_guess)
      high_guess_err = calc_error(guess: high_guess)
      low_guess_err = calc_error(guess: low_guess)

      if ref_err > high_guess_err # high_guess is a better guess
        ref_guess = high_guess
      else if ref_err > low_guess_err # low_guess is a better guess
        ref_guess = low_guess
      else
        # ref_err is unchanged, we should finish
        break
      end
    end

    puts "done"



  end

  # given an x value, predict y using y = x * slope
  def predict(x)
    x * @slope
  end


  ##
  # @param [Array<Float>] xs
  # @param [Array<Float>] ys
  # @param [Float] guess
  # @return Float
  #
  # Calculates the total squared error between guess Y and actual Y for each X
  #
  # Uses the simplifying assumption that the guess line goes through 0,0
  #
  def calc_error(guess:)
    errors = @xs
      .zip(@ys)
      .map do |x, y|
        y_guess = x * guess
        (y - y_guess) ** 2
      end

    # p ["Individual errors", errors]
    total_error = errors.sum

    # p ["total error", total_error]
    total_error
  end

  private

  def loadtxt(file_path, skip_rows: 0)
    lines = File.readlines(file_path, chomp: true)

    lines[skip_rows..]
      .map { |line| line.split(/\s+/).map { |x| Float(x) } }
  end
end

RSpec.describe "#calc_error" do
  it "works" do
    expect(1).to eq(1)

  end
end
