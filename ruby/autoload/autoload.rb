
module Car
	puts "evaluating Car"

	# returns nil because no autoloading required for this constant
	p autoload?(:Engine)

	autoload(:Engine, "./engine.rb") # second arg is whatever you would pass to Kernel#require

	# returns "./engine.rb" because the constant has been registered for
	# autoloading but not yet loaded
	p autoload?(:Engine)

	# referencing the Engine constant will trigger ruby to actually load it
	puts "about to ref Engine"
	Engine

	# returns nil because no autoloading required for this constant
	p autoload?(:Engine)
end
