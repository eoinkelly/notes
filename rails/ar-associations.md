# Assocations

## Association proxies

    http://api.rubyonrails.org/classes/ActiveRecord/Associations/CollectionProxy.html

Association proxies in Active Record are middlemen between the object that
holds the association and the actual associated object:

They allow AR to not load other objects from the DB until they are needed

* @owner
    * the object that holds the association
* @target
    * the actual associated object
* @reflection
    * The kind of association any proxy is about. That's an instance of the
      class ActiveRecord::Reflection::AssociationReflection.

```ruby
# pseudo code

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


```ruby
class CourseIntroduction < ActiveRecord::Base

  # param {object} = the instance of the model we are defined in
  # self = an instance of ActiveRecord_Relation that is part of Page (the other end of the relationship)
  has_many :pages, ->(obj) {
    # self and the param are NOT the same in here
    puts self #
    puts self.class # Page::ActiveRecord_Relation
    puts obj # the current instance of CourseIntroduction
    puts obj.class # CourseIntroduction
    positioned
  }, as: :page_collection, dependent: :destroy
end
```

Aside: neat way of implementing JS's 'arguments' in ruby

    args = method(__method__).parameters.map { |arg| arg[1] }
    puts "Method failed with " + args.map { |arg| "#{arg} = #{eval arg}" }.join(', ')

=============================================
=============================================
=============================================
=============================================
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

# Scopes

In old days Rails provided the find method and you could pass it a hash of conditions

```ruby
Book.find(:all, {
    conditions: ['title LIKE ?', '%bob%'],
    order: 'title'
})

```

These were not reusable


Rails 2.1 provided "named scopes" via `named_scope` This was improved because

1. it allowed for reuse
2. it allowed for composition and chaining

```ruby
# named scopes example
class Author < ActiveRecord::Base
    # note: scope is a hash
    named_scope :published, { :conditions => 'books_count > 0' }

    # this scope is a lambda because it needs to take an arg
    named_scope :by_first_name, lambda { |name|
        # returns a hash with conditions for the scope
        { :conditions => ['first_name = ?', name] }
    }
end

Author.published.by_first_name('bob')
```


Rails 3
    named_scope becomes scope
    converted the options form the scope hash into sepearate "query methods"
    :conditions becomes #where
    :order becomes #order
    all the query methods are still composable by chaining together

```ruby
# named scopes example
class Author < ActiveRecord::Base
    # note: scope is a hash
    scope :published, where('books_count > 0')

    # this scope is a lambda because it needs to take an arg
    scope :by_first_name, lambda { |name|
        # returns a hash with conditions for the scope
        where(['first_name = ?', name])
    }
end

Author.published.by_first_name('bob')
```

### An example where using raw SQL is not ideal

LIKE is case sensitive on Postgres but not other Dbs
Postgres provides ILIKE for case insensitive search but other dbs do not support ILIKE
So we should arel for queries involving LIKE to the x-db compliance - we need this especially if we are writing a library

```ruby
# named scopes example
class Author < ActiveRecord::Base

    def self.name_contains(name)
        where('name LIKE ?', "%#{name}%") # <-- problem because LIKE
    end
end

Author.published.by_first_name('bob')
```

the arel version

```ruby
# named scopes example
class Author < ActiveRecord::Base

    def self.name_contains(name)
        where(self.arel_table[:name].matches("%#{name}"))
    end
end

Author.published.by_first_name('bob')
```


An example of how raw SQL can break merging scopes
```ruby
class Author < ActiveRecord::Base
    scope :by_name, ->(name) { where('name = ?', name) }
end

class Book < ActiveRecord::Base
    scope :by_name, ->(name) { where('name = ?', name) }
end

Author.by_name('Sun Tzu').joins(:book).merge(Book.by_name('The art of war')) # <-- Problem
```
