# jeg2 talk

*  can have rails app in one file
*  rake notes
    *  rake notes
    *  rake notes:todos
    *  rake notes:fixmes
*  rails console --sandbox
*  rails console
    *  helper object - can call helper methods on it
        *  ?? just their ones
*  use diff server in rails dev mode
*  migrations
    *  column types on migration command line default to string so you can omit them
    *  you can pass length on cmd line
        *  name:string{10}
    *  you can set indexes on command line e.g.
        *  name:string:index
        *  name:string:uniq
    *  you can do associations
        *  rails g resource article user:references
        *  rails g resource article user:belongs_to
*  you can tell which migrations have been applied
    *  rake db:migrate:status
*  he showed some code for a rake task to put csv directly in db
*  rails can serialise to CSV - meh. YAML is prob better
*  Model.pluck
    *  User.pluck(:name) # SELECT name FROM users
    *  User.uniq.pluck(:name) # does SELECT DISTINCT name FROM users
*  rails console
    *  Model.group(:name, :email).count # gives you a nice summary of the groupings
    *  User.instantiate( … ) # lets you specify id
*  Postgres
    *  joshsusser has a trick on his blog to disable the length restriction on :string types
    *  he shows how to do full text search
    *  GIN = generic inverted index
*  rails can fairly easily put each user in their own database by reloading a custom DB when loading e.g based on username
*  rails has a deep merge for hashes - see #deep_merge (rails uses this for params)
*  #except
    *  my_hash.except(:name, :foo) # returns a hash with the named keys removed
*  #reverse_merge
    *  merges in unless it is already in there (diff to #merge)
*  #inquiry
    *  env = Rails.env
    *  env.development? # true|false
    *  "foo".inquiry.foo? # => true
    *  JEG2 shows how to set up your models so they can respond to this too
*  ERB has comments <%# i do not become a HTML comment %>
*  you can use #tap on a variable in the view to delimit its scope in the view - need to watch again
*  rails can do grouped select (you don't need simple_form
*  routing
    *  rails can route exceptions to any rack app (including itself)
        *  so you can route to itself and handle 404s yourself
        *  you can mount any sinatra (or rack) app into your rails app
*  CSV - he showed a snippet for sending CSV to browser - handy for navigator future
*  rails can write files to the filesystem atomically (by writing to a tmp and then moving)

