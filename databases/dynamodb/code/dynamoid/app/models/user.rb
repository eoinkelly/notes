class User
	include Dynamoid::Document

	field :first_name
	field :last_name
	field :type # dynamoid will auto fill this based in the model class name
end