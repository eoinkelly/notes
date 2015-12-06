require_relative '../lib/bike.rb'
require_relative '../lib/mountain_bike.rb'
require_relative '../lib/road_bike.rb'

describe Bike

  it "should have 2 wheels" do
    @bike = Bike.new
    @bike.num_wheels.should_be(2)
  end

  # it "should have 1 seat" do
  # end

  # describe "#new" do
    # it "should create a bicycle" do
    #   @bike = Bicycle.new
    #   @bike.should_be_instance_of Bicycle
    # end
  # end
end