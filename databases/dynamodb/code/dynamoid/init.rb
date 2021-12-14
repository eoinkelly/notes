require 'dynamoid'

Dynamoid.configure do |config|
  # To namespace tables created by Dynamoid from other tables you might have.
  # Set to nil to avoid namespacing.
  config.namespace = 'eoin_dynamoid'

  # [Optional]. If provided, it communicates with the DB listening at the endpoint.
  # This is useful for testing with [DynamoDB Local] (http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tools.DynamoDBLocal.html).
  config.endpoint = 'http://localhost:8000'
end

module DynamoidReset
  def self.all
    Dynamoid.adapter.list_tables.each do |table|
      # Only delete tables in our namespace
      if table =~ /^#{Dynamoid::Config.namespace}/
        Dynamoid.adapter.delete_table(table)
      end
    end

    Dynamoid.adapter.tables.clear

    # Recreate all tables to avoid unexpected errors
    Dynamoid.included_models.each { |m| m.create_table(sync: true) }
  end
end

require_relative "./app/models/user"
require_relative "./app/models/admin"
