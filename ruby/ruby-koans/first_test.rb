require 'test/unit'

class TestSomething < Test::Unit::TestCase
  def test_assert
    assert true
    assert_equal 1, 1
    assert_equal 1, 1.0
  end
end


