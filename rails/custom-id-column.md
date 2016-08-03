# How to use a custom primary key with Rails

* In this case we are using the `channels.name` column as the primary key

Consider an example where we have Channels and Products

* A Channel
    * has many Products
    * uses its `name` column as the primary key
* A Product
    * has one Channel

The postgres docs say

> a primary key constraint is simply a combination of a unique constraint and a
> not-null constraint

so we add those constraints manually to create our custom primary key.

```ruby
class AddChannelsAndProducts < ActiveRecord::Migration
  def change
    create_table :channels, id: false, primary_key: :name do |t|
      t.string :name, null: false # null: create column with a "not null" constraint
      # other fields go here ...
      t.timestamps
    end

    add_index :channels, :name, unique: true # add a uniqueness constraint

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

  # we need a URL friendly version of `name` for urls e.g. if name is "Foo
  # $33.0%" it will become "foo-33-0"
  def to_param
    name.parameterize
  end
end


# app/models/product.rb
class Product < ActiveRecord::Base
  belongs_to :channel, primary_key: :name
end
```

```
# in rails console
Channel.first.products      # works
Channel.first.product_ids   # works

Channel.find("some-name")   # works

# rails provides #id and #id= methods which are now aliases for #name and #name=
Channel.first.id == Channel.first.name # => true
cc = Channel.first
cc.name # => "foo"
cc.id = "bar"
cc.id   # => "bar"
cc.name # => "bar"

Product.first.channel       # works
Product.first.channel_id    # works (returns the channel name string)
```
