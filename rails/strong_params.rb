require 'action_controller'

raw = {
  user: {
    username: 'john',
    age: 123
  },
  things: [12, 34, 58],
  nested: [12, { foo: 'bar' }, 58],
  controller: 'foo/bar',
  action: 'create'
}


# scalar values are permitted by just mentioning their key name
# array values are permitted by passing a hash with the array name as a key and the value of an empty array e.g. `ary_name: []`
# hash values are permitted by passing a hash with the hash name as key and an array of hash keys as permitted keys
# The confusing bit is that scalar values don't need a type

params = ActionController::Parameters.new(raw)

# p params.require(:user) # returns the value of user key
# p params.require(:action) # returns the value of action key
# p params.require(:action, :controller) # ERROR can only give #require 1 arg

# permit

# p params.permit(:action) # returns a hash containing given key and its value
# p params.permit(:action, :controller) # returns a hash containing given keys and their values
#
# p params.permit(:action, :controller, :things) # does not return things
# p params.permit(:action, :controller, things: []) # returns things
# p params.permit(:action, :controller, { things: [] }) # returns things
#
# # puts "*** trying with nested hash ***"
# # p params.permit(:action, :controller, :user) # does not return user at all
# p params.permit(:action, :controller, { user: {} } )
# # p params.permit(:action, :controller, [:user])
# p params.permit([:action]) # same as above
# p params.permit([:action, :controller, :things]) # expects things to be scalar
# p params.permit([:action, :controller, things: []]) # expects things to be array of scalars
# p params.permit([:action, :controller, nested: []]) # expects things to be array of scalars
p params.permit([:action,
                 :controller,
                 { things: [] },
                 { user: [:username, :age] }
                ])
