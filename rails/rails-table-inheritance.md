# Single Table Inheritance

STI consists of reusing the same table to express a variety of models which
share similar attributes.

- generally it is thought to be a bad idea in a rails app

* use for models that share a _lot_ of the same stuff - if they are more
  different than alike use separate tables

- lots of nulls in the table
- potentially large table
- really awkward if you have to add a column to the table that is not shared
- gets difficult if your classes start having sublclasses (even more nulls)
- no metadata to identify which attributes belong to which subtypes = ok if you
  have
    - only a few sub-types
    - need to use a single-table database access pattern like active-record

ActiveRecord

- supports it
- parent inherits from ActiveRecord::Base
- children inherit from parent
- keeps all attributes in the same table
- usese special 'type' column to differentiante between children (rails g
  migration Parent ... type:string ...)

options: this
http://blog.poteland.com/blog/2013/05/09/single-table-inheritance-vs-class-table-inheritance/?utm_source=rubyweekly&utm_medium=email
does it using sequel has a dirty implementation for active::record
http://nathanmlong.com/2013/05/better-single-table-inheritance/

http://stackoverflow.com/questions/8884364/pros-and-cons-of-single-table-inheritance-for-assets-in-rails
good blog on how to do STI in rails:
http://www.christopherbloom.com/2012/02/01/notes-on-sti-in-rails-3-0/

# Class Table Inheritance

allegedly fiddly to do in rails

- mimics inhertiance
- http://techspry.com/ruby_and_rails/multiple-table-inheritance-in-rails-3/

# Concrete Table Inheritance

- just create separate models and be ok with the duplication of data in the DB

* hard to tell common attributes from subtype specific attributes - a new
  programmer will not be able to tell by looking at DB
* makes searching all subtypes harder Conclusion Use concrete table inheritance
  if you don't need to search much
