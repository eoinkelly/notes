require 'test_helper'

class ArrayTest < EdgeCase::TestCase

  def test_basic_arrays
    food = [:peanut, :button, :and, :jelly]
    assert_equal __, food[0]
    assert_equal __, food.size
  end

  def test_array_access
    food = [:peanut, :button, :and, :jelly]
    assert_equal __, food.first
    assert_equal __, food.last
    assert_equal __, food[0]
    assert_equal __, food[2]
    assert_equal __, food[(food.size() - 1)]
  end

  def test_arrays_with_other_objects
    food = [:peanut, :button, :and, :jelly, 1, nil]
    assert_equal __, food.size
    assert_equal __, food.last
    assert_equal __, food[5]
  end

  def test_adding_to_an_array_with_shovel_shovel
    food = [:peanut, :button, :and, :jelly]
    food << 'sandwich'
    assert_equal __, food.size
    assert_equal __, food.first
  end

  def test_adding_to_an_array_with_push
    food = [:peanut, :button, :and, :jelly]
    food.push('sandwich')
    assert_equal __, food.last
  end

  def test_adding_to_an_array_with_unshift
    food = [:peanut, :button, :and, :jelly]
    food.unshift('a')
    assert_equal __, food.first
  end

end

