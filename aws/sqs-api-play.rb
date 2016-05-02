require "aws-sdk"
require "pry"

sqs_client = Aws::SQS::Client.new(region: "ap-southeast-2") # Sydney

p sqs_client.operation_names # dump operations
# => [:add_permission, :change_message_visibility, :change_message_visibility_batch, :create_queue, :delete_message, :delete_message_batch, :delete_queue, :get_queue_attributes, :get_queue_url, :list_dead_letter_source_queues, :list_queues, :purge_queue, :receive_message, :remove_permission, :send_message, :send_message_batch, :set_queue_attributes]

# each operation takes a hash and retursn a response object
# errors are returned under Aws::SQS::Errors::ServiceError

# @return #<struct Aws::SQS::Types::ListQueuesResult
queue_url = sqs_client.list_queues.queue_urls.first

# rquest params in the docs are PascalCase but should be snake_case for the ruby api

# @return #<struct Aws::SQS::Types::SendMessageResult
_send_msg_result = sqs_client.send_message(message_body: "hello first msg", queue_url: queue_url)

# note how we send message attributes through
message_attributes = {
  name: {
    data_type: "String",
    string_value: "Eoin kelly"
  }
}

# @return <struct Aws::SQS::Types::SendMessageResult
_send_msg_result_2 = sqs_client.send_message(message_body: "hello new msg",
                                             queue_url: queue_url,
                                             message_attributes: message_attributes)
