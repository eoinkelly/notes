# Assocations

## Association proxies

    http://api.rubyonrails.org/classes/ActiveRecord/Associations/CollectionProxy.html

Association proxies in Active Record are middlemen between the object that
holds the association and the actual associated object:

They allow AR to not load other objects from the DB until they are needed

* @owner
    * the object that holds the association
* @target
    * the actual associated object, known as the
* @reflection
    * The kind of association any proxy is about. That's an instance of the
      class ActiveRecord::Reflection::AssociationReflection.

```ruby
class PseudoAssociationProxy
  def initialize
    @target
    @owner
    @reflection # instance of ActiveRecord::Reflection::AssociationReflection
  end

# delegates unknown methods to @target via method_missing
end

class Page
  has_many :authors
end

p = Page.first
proxy = page.authors

# QUESTION: is proxy above really an instance of an association proxy that is hard to get at ???
    or does page.authors really return the @target object?

```

Proxies allow you to "scope" find and create operations

p = Page.first
p.authors.where(name: 'blah')

http://pivotallabs.com/advanced-proxy-usage-part-i/

There are 6 types of association

1. belongs_to
1. has_one
1. has_one :through
1. has_many
1. has_many :through
1. has_and_belongs_to_many

* Associations are a set of macro-like **class methods** for tying objects
  together through foreign keys.
    * => they exist on the model class, not on instances of the models
    * `User.addresses` not `User#addresses`
* They express relationships between object s
* Each macro adds a number of methods to the **class** which are specialized
  according to the collection or association symbol and the options hash.

ActiveRecord makes methods for you from the attributes in the database table
These "generated attributes" are added to your model using an anonymous module.

* Association methods are added to `Foo` class from a named `Foo::GeneratedAssociationMethods` module
* The methods are **class** methods on this module (as they are class methods in `Foo`)
* The "generated association" methods are added _after_ the "generated
  attribute" module so the association methods will override the attribute
  methods if there is a clash i.e. if you have an association `bar` and an
  attribute `bar` you will get the association every time.

Generally `Foo::GeneratedAssociationMethods` will be the first ancestor in the `Foo.ancestors` chain

QUESTION: how do I get a reference in pry to something in the ancestor tree?

```
pry(main)> Page.ancestors
[
    [ 0] class Page < ActiveRecord::Base {
                          :id => :integer,
                        :type => :string,
               :app_layout_id => :integer,
                     :content => :text,
                  :created_at => :datetime,
                  :updated_at => :datetime,
                 :grouping_id => :integer,
                      :header => :string,
                    :position => :integer,
          :page_collection_id => :integer,
        :page_collection_type => :string,
                       :audio => :string,
                :instructions => :string,
                   :course_id => :integer,
                 :show_header => :boolean
    },
    [ 1] Page::GeneratedAssociationMethods,
    [ 2] #<Module:0x007f9edaf1fda8>,
    [ 3] #<#<Class:0x007f9edaf3da38>:0x007f9edaf3db00>,
    [ 4] ActiveRecord::Base,
    ...
```

QUESTION: What does "cardinality" mean exactly?

## Caching

The association methods are built on top of a cache that keeps the result of the most recent query available for further operations.
You can discard this cache and force hitting the DB again by passing `true` to association methods
Note that it does not just skip the cache as a one-time thing - it refereshes the cache too

```ruby
# get addresses and skip cache
User.addresses(true)
```


### Customising associations

Associations are built from Relations, and you can use the Relation syntax to customize them

what parasm dos the lambda get?
what is self in the lambda? it seems to be the
does the lambda have to return a Relation ???

Is it correct to say:
    The return value of the lambda is expected to be a relation and it is simply merged with whatever relation the calling code has made


* passing a lambda to an association sets a "scope" on it
    QUESTION: does it have to be lambda, can be proc?
* the return value of the lambda is ??? an ActiveRecrod::Relation
    QUESTION: does it have to be?

The returned ActiveRelation is combined with whatever query is going on when
somebody asks for pages e.g.

```
Grouping.pages
# becomes
Grouping.pages.order(:position).includes(:app_layout)
#

Grouping.pages.where(name: 'foo')
# becomes
Grouping.pages.where(name: 'foo').order(:position).includes(:app_layout)
#
```

* ActiveRecord is smart enough to not do the join if you don't need it

http://api.rubyonrails.org/classes/ActiveRecord/Associations/ClassMethods.html#label-Customizing+the+query
```ruby
class Grouping < ActiveRecord::Base
  acts_as_list

  has_many :pages, -> { positioned.includes(:app_layout) }, as: :page_collection, dependent: :destroy

  scope :positioned, -> { order(:position) }
end
```

