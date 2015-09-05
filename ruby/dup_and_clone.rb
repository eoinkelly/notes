
class Thing
  def initialize_copy(new_copy)
    puts 'initialize_copy'
    puts "self: #{self.object_id}"
    puts "new_copy: #{new_copy.object_id}"
    super
  end

  def initialize_dup(new_copy)
    puts 'initialize_dup'
    puts "self: #{self.object_id}"
    puts "new_copy: #{new_copy.object_id}"
    super
  end

  def initialize_clone(new_copy)
    puts 'initialize_clone'
    puts "self: #{self.object_id}"
    puts "new_copy: #{new_copy.object_id}"
    super
  end
end

t = Thing.new
puts '%%%%%%%%%%%%%%%%%%%%%%%%%'
t.clone
# initialize_clone
# self: 70098840735460
# new_copy: 70098840735520
# initialize_copy
# self: 70098840735460
# new_copy: 70098840735520

puts '%%%%%%%%%%%%%%%%%%%%%%%%%'

t.dup
# initialize_dup
# self: 70098840735100
# new_copy: 70098840735520
# initialize_copy
# self: 70098840735100
# new_copy: 70098840735520
