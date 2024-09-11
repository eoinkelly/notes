require 'aws-sdk-dynamodb'

# for local use w docker container:
Aws.config[:dynamodb] = {endpoint: 'http://localhost:8000'}
dynamodb = Aws::DynamoDB::Client.new(endpoint: 'http://localhost:8000')

# for remote AWS hosted:
# dynamodb = Aws::DynamoDB::Client.new

dynamodb.create_table({
  table_name: 'Products',
  attribute_definitions: [
    {attribute_name: 'account_id', attribute_type: 'S'},
    {attribute_name: 'product_id', attribute_type: 'S'}
  ],
  key_schema: [
    {attribute_name: 'account_id', key_type: 'HASH'},
    {attribute_name: 'product_id', key_type: 'RANGE'}
  ],
  provisioned_throughput: {
    read_capacity_units: 1,
    write_capacity_units: 1
  }
})

dynamodb.put_item({
  table_name: 'Products',
  item: {
    'account_id' => '93d0',
    'product_id' => '0001',
    'title' => 'Blue & Black Dress',
    'colours' => Set.new(['blue', 'black']),
    'likes' => 0
  }
})

dynamodb.put_item({
  table_name: 'Products',
  item: {
    'account_id' => '93d0',
    'product_id' => '0002',
    'title' => 'White & Gold Dress',
    'colours' => Set.new(['white', 'gold']),
    'likes' => 0
  }
})

response = dynamodb.get_item({
  table_name: 'Products',
  key: {
    'account_id' => '93d0',
    'product_id' => '0001'
  }
})

item = response.item

p item
# {"likes"=>0.0, "account_id"=>"93d0", "colours"=>#<Set: {"black", "blue"}>, "product_id"=>"0001", "title"=>"Blue & Black Dress"}
# p item.class
# Hash