require File.expand_path(File.dirname(__FILE__) + '/edgecase')

class AboutNil < EdgeCase::Koan
  def test_nil_is_an_object
    assert_equal true, nil.is_a?(Object), "Unlike NULL in other languages"
  end

  def test_you_dont_get_null_pointer_errors_when_calling_methods_on_nil
    # What happens when you call a method that doesn't exist.  The
    # following begin/rescue/end code block captures the exception and
    # makes some assertions about it.
    begin
      nil.some_method_nil_doesnt_know_about
    rescue Exception => ex
      # What exception has been caught?
      assert_equal NoMethodError, ex.class

      # What message was attached to the exception?
      # (HINT: replace __ with part of the error message.)
      assert_match(/undefined method/, ex.message)
    end
  end

  def test_nil_has_a_few_methods_defined_on_it
    assert_equal true, nil.nil?
    assert_equal "", nil.to_s
    assert_equal "nil", nil.inspect

    # THINK ABOUT IT:
    #
    # Is it better to use
    #    obj.nil?
    # or
    #    obj == nil
    # Why?

    # obj.nil? can be overridden by the obj class which has better knowledge about obj
    # however, is it meaningful or a good idea to play with the notion of nil - surely nil should always be just nil

    # what we know about nil:
    # * nil is ruby's "nothing" value.
    # * it evaluates to false in conditionals

    # http://www.themomorohoax.com/2009/05/21/when-to-use-nil-in-ruby-methods

    # what are we trying ot accomplish wiht the above code?
    # 1. test if obj exists
    # 2. test if obj is
    # what tasks would we use #nil? for?
    # *
  end

end
