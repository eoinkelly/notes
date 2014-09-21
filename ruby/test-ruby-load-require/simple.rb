class Simple
  SOME_CONST = 2333
  @@some_class_var = 345
  def initialize(x = 12, y = 3)
    @x = x
    @y = y
  end

  def do_stuff
    'stuff'
  end

  def other_stuff
    'stuff'
  end

  private

  def secret_stuff
  end

  protected

  def protected_stuff
  end
end
