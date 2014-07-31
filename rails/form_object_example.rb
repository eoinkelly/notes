class Forms::Assessment

# this is an instance of the presenter pattern
# a class representation of the state of the view.
# logic can be moved from the controller to here
# so this is a way of simplifying *controllers* not models

# presenter is a class that represents the state of the *view* - it respresents more than just a form
# this model just represents the form - it is a subset of a presenter that just cleans up the form part of the view - the controller still does some work

# you could also use ruby forwardable to delegate calls to the underlying models
  # this is just a way of writing less code

  # #################################################
  # Active Model interface
  # #################################################

  # ActiveModel plumbing to make `form_for` work
# ? are all these required?
# notice extend vs. include
  extend ActiveModel::Naming
  include ActiveModel::Conversion
  include ActiveModel::Validations

  # this name will appear in params[:assessment] and in form_for stuff
  # josh susser has a method that will rename this model
  def self.name
    "Assessment"
  end 


  def persisted?
    false
  end

  def new_record?
    true
  end

  def to_model
    self
  end

  def to_key
    # By default rails will use the same id for all new records <model-name>_new - this makes it arkwrad in the DOM if you have more than one new object. If we return object_id we get a nice unique id that we can use in the DOM. Don't 

    [object_id] # josh uses this
    # other implementations use this
    # nil
  end

  def to_param
    nil
  end

  # #################################################
  # Custom interface for my controllers
  # #################################################

  # this object must conform to the following interfaces:
  # * activemodel
  # * whatever methods I use from the controller
  # * create
  # Forms::Assessment.create(params[:domain]) 

  def create
    return unless valid?

    model = UnderlyingModel.create!(
      event: event,
      data_json: {
        name: name,
        age:  age.to_i,
      }.to_json
    )

    model 
  end

  def save
    return false unless valid?
    if create_objects
      UserMailer.deliver_user_registered(user)
      IpLogger.log(user, ip_address)
    else
      false
    end
  end

  # #################################################
  # Initialization 
  # #################################################

  # approach 3 for auto-making accessors

  # josh susser magic initializer
  def attributes=(attrs)
    attrs && attrs.each_pair { |name, value| self.send("#{name}=", value) }
  end

  # approach 1 for auto-making accessors

  # this creates accessors for each item in a has_many association
  # good when you do't know many items there will be
  # + allows the form to avoid fields_for 
  def add_answer_accessors!
    event.questions.each do |q|
      attr = :"answer_#{q.id}"
      instance_eval <<-RUBY
        def #{attr};     answers[#{q.id}]; end
        def #{attr}=(x); answers[#{q.id}] = x; end
      RUBY
    end
  end

  # approach 2 for auto-making accessors
  ATTRIBUTES = [:name, :email, :terms_of_service, :anti_bot_question_id, :anti_bot_answer, :ip_address]

  attr_accessor *ATTRIBUTES

  def initialize(attributes = {})
    ATTRIBUTES.each do |attribute|
      send("#{attribute}=", attributes[attribute])
    end
  end

  # manually getter
  def business
    @business ||= Business.new(:name => name)
  end

  # #################################################
  # Validations 
  # #################################################

  # can validate the attributes we get from the form here and return nice errors to it

  # #################################################
  # Other 
  # #################################################

  # some seem to have an idea of a attr_writers that takes a string
  # representation of an attribute and actually saves the underlying type (boolean,
  # int etc.)
  # http://rhnh.net/2012/12/03/form-objects-in-rails

end
