App.ComponentForTypeView = Ember.ContainerView.extend(Ember._Metamorph, {
  item: null,
  onDelete: 'onDelete',
  onSelect: 'onSelect',

  childViews: function () {
    return [this.componentForType()];
  }.property(),

  changeChild: function () {
    this.clear();
    this.pushObject(this.componentForType());
  }.observes('item.type'),

  componentForType: function () {
    type = this.get('item.type');

    container = this.get('container');
    componentLookup = container.lookup('component-lookup:main');
    itemComponent = componentLookup.lookupFactory(type, container);

    itemComponent.create({
      item: this.get('item'),
      onDelete: this.get('onDelete'),
      onSelect: this.get('onSelect')
    });
  }
});

Ember.Handlebars.helper('component-for-type', ComponentForTypeView);
