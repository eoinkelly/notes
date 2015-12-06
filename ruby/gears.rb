# Attempt 1
# #########

# class Gear
#   attr_reader :chainring, :cog

#   # initialize method is always private
#   def initialize(chainring, cog)
#     @chainring = chainring
#     @cog = cog
#   end

#   def ratio
#     # If you divide two integers in ruby the result is an iteger. By converting
#     # one number to a float, we ensure that the answer will be a float.
#     @chainring / @cog.to_f
#   end
# end

# # Because gear inherits from Object it can actually respond to a lot of messages other than the ratio one it defines itself.

# puts Gear.new(52,11).ratio
# puts Gear.new(30,27).ratio

# Attempt 2
# #########
# class Gear
#   attr_reader :chainring, :cog, :rim, :tire

#   # initialize method is always private
#   def initialize(chainring, cog, rim, tire)
#     @chainring = chainring
#     @cog = cog
#     @rim = rim
#     @tire = tire
#   end

#   def ratio
#     # If you divide two integers in ruby the result is an iteger. By converting
#     # one number to a float, we ensure that the answer will be a float.
#     @chainring / @cog.to_f
#   end

#   def gear_inches
#     ratio * (rim + (tire * 2))
#   end
# end

# puts Gear.new(52,11, 26, 1.5).ratio
# puts Gear.new(30,27, 24, 1.25).ratio

# Attempt 3
# #########
class Gear

  # A nice side-effect of having readers for each var is that we can use the
  # reader within the class
  # Of course this makes these vars accessible to public which might be a problem?

  # private is defined as methods (or variables!) that can *only* be called implicityly - you cannot pass a receiver to them, even self
  # this means that "private" stuff is available to child classes
  private
    attr_reader :chainring, :cog, :rim, :tire

  public

  # initialize method is always private
  def initialize(chainring, cog, rim, tire)
    @chainring = chainring
    @cog = cog
    @rim = rim
    @tire = tire
  end

  def ratio
    # If you divide two integers in ruby the result is an iteger. By converting
    # one number to a float, we ensure that the answer will be a float.
    chainring / cog.to_f
  end

  def gear_inches
    ratio * Wheel.new(rim, tire).diameter
  end
end

class Wheel
  attr_reader :rim, :tire

  def initialize(rim, tire)
    @rim = rim
    @tire = tire
  end

  def diameter
    rim + (tire * 2)
  end
end

puts Gear.new(52,11, 26, 1.5).ratio
puts Gear.new(30,27, 24, 1.25).ratio

