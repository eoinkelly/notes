# How to use a custom primary key with Rails

* In this case we are using the `channels.name` column as the primary key

```ruby
class AddPimTables < ActiveRecord::Migration
  def change
    create_table :channels, id: false, primary_key: :name do |t|
      t.string :name, null: false
      # other fields go here ...
      t.timestamps
    end

    add_index :channels, :name, unique: true

    # products uses a standard integer id
    # It is demonstrating here how to have a relationship to a table with a
    # custom primary key
    create_table :products do |t|
      # other columns go here ...
      t.string :channel_id
    end
  end
end
```

```
# app/models/channel.rb
class Channel < ActiveRecord::Base
  self.primary_key = :name
  has_many :products
end


# app/models/product.rb
class Product < ActiveRecord::Base
  belongs_to :channel, primary_key: :name, foreign_key: :channel_id
end
```

```
# in rails console
Channel.first.products      # works
Channel.first.product_ids   # works

Product.first.channel       # works
Product.first.channel_id    # works (returns the channel name string)
```
