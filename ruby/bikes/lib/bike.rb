class Bike
  attr_reader :size, :chain, :tire_size

  def initialize(args = {})
    # use fetch instead of
    # @size = args[:size] || 10 # works but not for boolean options or if we need to distingues between false and nil
    # also relies on hash returning nil for missing keys

    # @blah = args[:foo] # use this form if you actually want @blah to be nil if we didn't send a parameter
    # @blah = args.fetch(:foo, nil) # does same thing. More explicit but wordier

    @size = args.fetch(:size, 10)
    @chain = args.fetch(:chain, default_chain) # Template Method pattern
    @tire_size = args.fetch(:tire_size, default_tire_size) # Template Method pattern
    post_initialize(args)
  end

  # a hook for subclasses to optionally contribute to initialization without requiring them to call super
  # now the subclasses don't know *when* this will be called - they just know what to do to customise initialization
  def post_initialize(args)
    nil
  end

  # Template Method Pattern.
  # By providing a default value, I am documenting that subclasses can optionally override this method
  def default_chain
    '10-speed'
  end

  # Template Method Pattern.
  # Raising an exception explicitly documents that subclasses are required to override this method
  def default_tire_size
    raise NotImplementedError, "This #{self.class} cannot respond to:"
  end
end

class RoadBike < Bike
  attr_reader :tape_color

  def post_initialize(args)
    @tape_color = args.fetch(:tape_color, "red")
  end

  def default_tire_size
    44
  end
end

class MountainBike < Bike
  # def initialize
  #   # super
  # end

  def default_tire_size
    23
  end
end


class RecumbentBike < Bike
  # def initialize
    # super
  # end
end

# notice that all subclasses send super in the same place => duplicate code
# all subclasses have to remember to call super at the right time => easy to forget

a = RoadBike.new
p a
b = RecumbentBike.new
p b