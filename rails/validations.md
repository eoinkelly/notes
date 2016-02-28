# Validations


ActiveModel::Validations does the validation
ActiveRecord does the type coercion (of types from the form to what the DB needs)


* should association validations be on the foo_id or on the foo?
* can I have two models which validate their association from both ends?


## What is the best way to reliably validate the presence of an association?

1. Set `NOT NULL` constraint on the foreign_key column - this is the safety net
2. Add `validates :other_model, presence: true` to your model
    * note we are validating the presence of `other_model` not `other_model_id`

option: consider using foreign key constraints in the DB to enforce referential integrity
    http://edgeapi.rubyonrails.org/classes/ActiveRecord/ConnectionAdapters/SchemaStatements.html#method-i-add_foreign_key

Does this work?

```ruby
class Foo
  has_one :bar
  valdiates :bar, presence: true
end
```

My question really is "what does the presence validation actually check?"

## How do I run validations of an associated object when I am saved?

Note: we are not talking about validating the existance of the assocation here - we are talking about validating the associated object itself automatically.

* Use `validates_associated`
* Options
    * :on
        * specify the "validation contexts" where this validation is active
        * contexts are
            * :create
            * ??? others ???
    * :if
        * specify a method, proc or string to call to decide whether to validate the associated object
        * whatever you run it should return true|false
    * :unless
        * inverse of :if
http://api.rubyonrails.org/classes/ActiveRecord/Validations/ClassMethods.html#method-i-validates_associated

TODO: learn about "validation contexts" - seems you can create custom ones?

## Should I always add inverse_of option to an association?

No. Since rails 4.1 `inverse_of` is automatically added to most associations

* Automatic inverse detection: https://github.com/rails/rails/pull/10886/files
* Telling rails about the inverse of associations helps it not load multiple
  copies of the models into memory when fetching associated records
* only works on
    * `has_many`
    * `has_one`
    * `belongs_to`

There are some options on the above associations which will break automatic
inverse detection so you still have to explicitly provide `inverse_of` options
e.g.

* custom class name
* custom foreign key
* others ???

