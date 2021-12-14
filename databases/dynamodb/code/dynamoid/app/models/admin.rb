class Admin < User
	include Dynamoid::Document

	field :role
end