# Rails associations


Since you can create models through associations you should always do it that way if possible rather than passing in the association ID as part of a mass assignment hash - it is safer.

* is making an assication mass assignable an anti-pattern? he kind of implies it is ...
    * does it yield up too much control to the controller?
* CRUDing models through associations seems to be a good pattern

## visualising belongs_to
belongs_to says "I have the id of the other side on me (it belongs to me) so I can find it"
  imagining a person who has a "token" in their pocket that will identify the other person that they know about

# working through associations as a good pattern

you can build through an association to set it up at the same time
card = deck.cards.new # works and automatically sets the deck_id to be the deck
you can get around mass assigment protection by building the object through associated objects because you aren't explicitly setting the id of the associated model

you can find cards through an association which acts as a filter on the search, also security checking

deck.cards.find(2) vs Card.find(2) # the latter does not check that the card is associated with deck
