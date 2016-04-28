require "aws-sdk"
require "pry"

dynamo_db = Aws::DynamoDB::Resource.new(region: "ap-southeast-2") # Sydney

table_spec = {
  table_name: "Users",
  attribute_definitions: [
    {
      attribute_name: "ID",
      attribute_type: "N"
    }
  ],
  key_schema: [
    {
      attribute_name: "ID",
      key_type: "HASH"
    }
  ],
  provisioned_throughput: {
    read_capacity_units: 1,
    write_capacity_units: 1
  },
  stream_specification: {
    stream_enabled: true,
    stream_view_type: "NEW_IMAGE"
  }
}

table = dynamo_db.create_table(table_spec)

dynamo_db.client.wait_until(:table_exists, table_name: "Users")

example_item = {
  item: {
    "ID" => 123_456,
    "FirstName" => "Snoop",
    "LastName" => "Doug"
  }
}

table.put_item(example_item)

scan_output = table.scan(limit: 50, select: "ALL_ATTRIBUTES")

scan_output.items.each do |i|
  id = i["ID"]
  first_name = i["FirstName"]
  last_name = i["LastName"]

  puts "ID:         #{id.to_i}"
  puts "First name: #{first_name}"
  puts "Last name:  #{last_name}"
  puts
end
