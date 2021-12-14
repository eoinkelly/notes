require_relative "./init"

# Dynamoid uses uuid as id's not integers

# this scans the table for all with type=User and then issues batch requests to request batches of records
# User.delete_all
# Admin.delete_all
DynamoidReset.all

100.times do |n|
	User.create(
		first_name: "UserFirst #{n}",
		last_name: "UserLast #{n}"
		# the type field is automatically filled in by Dynamoid
	)
end

100.times do |n|
	Admin.create(
		first_name: "AdminFirst #{n}",
		last_name: "AdminLast #{n}",
		role: "Bossy boss"
		# the type field is automatically filled in by Dynamoid
	)
end