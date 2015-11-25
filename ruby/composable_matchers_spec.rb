require "rspec"

puts RSpec::Matchers.instance_methods

RSpec.describe "Composable matchers" do
  describe "be_within" do
    it "basic use" do
      expect(12.4).to be_within(0.2).of(12.5)

      expect([12.4, 14.3]).to start_with(a_value_within(0.2).of(12.5))
      expect([12.4, 14.3, 33.4]).to start_with(
        a_value_within(0.2).of(12.5),
        a_value_within(0.1).of(14.3),
        a_value_within(0.5).of(33.3)
      )
    end
  end
end
