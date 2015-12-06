# Testing class methods

class Parent

  def self.class_method_example
    puts "you called class_method_example successfully"
  end

  # def self.am_i_private?
  #   puts "you called am_i_private? successfully"
  # end

  # private_class_method :am_i_private?

  def initialize
    puts "in Parent.initialize"

    # call class method from within our own class defn. block:

    # class_method_example # Fails
    # self.class_method_example # Fails
    Parent::class_method_example # Works

    # Parent::am_i_private? # Fails
    # am_i_private?
  end

  def instance_method_1
    puts "In Parent#instance_method_1"
    Parent::class_method_example # Works
  end
  def instance_method_2
    puts "In Parent#instance_method_2"
  end

end

class Child

  def initialize
    puts "in Child.initialize"

    # Test: call class method from within subclass class defn. block
    # class_method_example # Fails
    Parent::class_method_example # Works
  end

  def instance_method_3
    puts "In Child#instance_method_3"
    Parent::class_method_example # Works
  end

  def instance_method_4
    puts "In Child#instance_method_4"
  end

end


p = Parent.new
# p.class_method_example # Fails
Parent::class_method_example # Works
p.instance_method_1

c = Child.new
c.instance_method_3

