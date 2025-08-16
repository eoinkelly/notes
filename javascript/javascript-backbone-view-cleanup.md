There are 5 things needed to close a view

1. unbind any DOM events attached to the view's DOM node
2. unbind any backbone events that it is listening to on other objects (call
   .stopListening() for each .listenTo())
3. remove the HTML
4. unbind an backbone event handlers it has attached to other objects (call
   .off() for each .on())
5. remove any events-handlers that other objects might have put in our list

PART I Delgates to $(this.el).remove() which unbinds DOM events and removes HTML
(steps 1 & 3) Calls this.stopListening() which removes our event handlers from
other objects event lists (step 2) this.remove();

PART II Removes all _backbone_ events bound by other objects (or possibly the
view itself) to this view (step 5)

this.off(); // or this.unbind();

PART III We have to manually remove our handlers from the event-lists of other
objects if we added them via .on() and not .listenTo()

all views need to call .close() when they are destroyed - they can't do this for
themselves so the router or an intermediary will have to all views still need to
implment a this.close() method where they manually remove anything they added
event handlers to with .on()

- every on() in a view should have an off() in the destructor of the same view
- listenTo() is called as part of this.remove() so you can specify that in the
  monkey-patch method

foo.on('event', bar.method, context) gives foo a reference to bar.method and a
reference to context so now foo knows about both of those objects if foo is
destroyed you dont have to do anything as bar and context don't keep a reference
to foo if bar or bar.method or context is destroyed then you need to remove the
reference from foo's event list

you should call .off() in the same object that you called .on() technically can
call it from any object tha has a refernce to both foo and bar

bar.listenTo(foo, 'event', bar.method) gives foo a reference to bar.method (and
bar's context? gives bar a reference to foo if bar is destroyed, you have to
remove the reference in foo if foo is destroyed you have to remove the reference
in bar the net effect of listenTo() is that both objects now have a reference to
each other so you have to delete the reference in the other if you delete either

you should call stopListening() in the same object that you called listenTo()
(or just call this.remove() which does call stopListening())
