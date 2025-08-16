# Ember Architecture

These are just rough notes copied and pasted from the dicussion forums

## controllers in ember 2

today's controllers become either:

service (non view backing, and singleton) component (view backing, and non
singleton)

Dropping the "name" controller, is to ease migration as it allows these entities
to live side-by-side without naming collision. Ultimately ember has several
controller specializations (Routes/Components/Services) each well suited for its
task, and each with its own life-cycle.

i typically look to the life-span and reach of an object to decide what it is,
in most apps the user or session controller should likely be a singleton shared
service.

## controllers today

Ember.Controller funcitons as a context object for the template

Ember.ObjectController is more like a decorater for the model as it magically
merges model properties with controller properties

## Data down, actions up

Visualise data flowing down from the model hook of the route
