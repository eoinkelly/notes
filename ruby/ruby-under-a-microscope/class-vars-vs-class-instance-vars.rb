class Parent 

  # constants and class level instance variables are implemented in the same way by ruby
  # you could think of a constant as a cliv that triggers a warning if you override it
  SOME_CONST = "parent constant"
  
  # each class has it's own copy of @class_instance_var
  @class_instance_var = "parent class instance var" 

  # this class and all it's children share this var. updates from any of them
  # will update the var for all
  @@class_var = "parent class var"

  def self.civ
    puts @class_instance_var
  end
  def self.cv
    puts @@class_var
  end
end

# class var value is shared between this class and all it's children
# class instance var values are different for each class

class Child1 < Parent
  @class_instance_var = "child class instance var" 
  @@class_var = "child class var"
end

Parent.civ
Parent.cv

Child1.civ
Child1.cv

Parent.civ
Parent.cv
