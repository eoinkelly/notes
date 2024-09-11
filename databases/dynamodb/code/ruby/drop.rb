require 'aws-sdk-dynamodb'

Aws.config[:dynamodb] = {endpoint: 'http://localhost:8000'}
dynamodb = Aws::DynamoDB::Client.new(endpoint: 'http://localhost:8000')

puts "dropping table"
dynamodb.delete_table(table_name: 'Products')