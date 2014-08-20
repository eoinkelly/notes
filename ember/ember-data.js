// TODO: invoke()
// this.get('posts').invoke('save');
// calls .save() on each element in the array

// TODO: research Ember.K, Ember.A

// TODO: research state machines

// Actions bubble up through the history of the state machine !!!

// Unloading Ember-data models
// ***************************

this.store.find('post', 1).unloadRecord(); // unload the record from memory

// how to handle unloading records that have children - customise unloadRecord()

Var Post = DS.Model.extend({
  unloadRecord: function () {
    this._super(); // call the standard unloadRecord() method

    this.eachRelationship(function (key, relationship) {
      if (relationship.kind === 'hasMany') {
        this.get(key).toArray().invoke('unloadRecord');
      } else {
        this.get(key).unloadRecord();
      }
    }, this);
  }
})

// kick everything off
this.store.find('post', 1).unloadRecord();


//
App.MyAwesomeComponent = Ember.Component.extend({

  didInsertElement: function () {
    // option 1
  },

  // Option 2:
  // * have multiple functions that run on the event
  // + you don't have to have a single huge event handler
  // + you can give the method a more meaningful name
  // + it might be easier to test - would not have to directly call the didInsertElement function
  // ? what order do they run
  doThing: function () {

  }.on('didInsertElement'),

  doSomeOtherThing: function () {

  }.on('didInsertElement')
})
