require 'aws-sdk-dynamodb'

Aws.config[:dynamodb] = {endpoint: 'http://localhost:8000'}
dynamodb = Aws::DynamoDB::Client.new(endpoint: 'http://localhost:8000')

r2 = dynamodb.scan(table_name: 'Products')

p all_items = r2.items