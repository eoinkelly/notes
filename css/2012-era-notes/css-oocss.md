# OOCSS

Inbox:
http://www.slideshare.net/briancavalier/oocss-for-javascript-pirates-at-jquerypgh-meetup
the grid lays out "container objects" not "content objects" by defn. an object
is a "container" if it appears in the grid think more about container and
contentobjects

## OOCSS for JS pirates slides

    separation of container from content
    separate structure from skin (use separate classes to apply structure and skin)
    separate identity from state (for JS)
        use different classes for state changes (base, special are "identity" classes, state1, state2 are "state" classes)

        <tag class="base special state1 state2"/>

there are "container CSS objects" and "content CSS objects"

content objects flow naturally container objects restrict where/how content
objects flow containers dont' have to be only grids and columns

content can be laid out differently by changing the container object/container
class the container and content objects don't have to be separate but they often
are

learn to love grids use grids to position content grid columns control content
width content height flows naturally

OOCSS objects do not correlate with back-end components/views - they are
completely independant

OOCSS _objects_ inheirt from other OOCSS _objects_ (like javascript)

We want loose coupling between JS and CSS we want to be able to change the
html/css without changing JS so: BAD: $('.my-thing ul').css('display', 'none');
GOOD: $('.my-thing ul').addClass('hidden'); stale selectors (2 types) 1. rules
that are no longer used at all (we want none of these) 2. rules that will be
used later (either after JS or on another page) - these are ok to have

## General OOCSS

1. We want our CSS to be predictable no matter where they are - objects should
   not change depending on where they are in the code e.g. h3 in main content
   should not be different from h3 in the aside.
2. Separate CSS structure and chrome
3. Use a grid system.
4. The key thing here is to identify 'Objects' or building block patterns in
   your site and architect with them.
5. Hacks are better than .ie6/.ie7 classes as hacks don’t change specificty e.g.
   the \* and \_ hacks are preferable.
6. Do not style IDs at all - do everything with classes - classes have a
   predictable specificity
7. Sections of HTML that appear repeatedly on the site without structural
   modification (known as "modules") can be considered selector roots, and from
   there you can scope the internal layout relative to that root. This is the
   basic tenet of object-oriented CSS.
8. Avoid !important except maybe maybe on leaf-nodes
9. style classes without their elements e.g. .content rather than div.content so
   that specificty is kept sane - can use the elements to drill down 0for more
   specific cases sometimes.
10. make all rules the same lenght so you can be confident about which will win.
    I.e. all rules that target a class should have the same length e.g. .mymod
    .hd .mymod .hd (this is good) Vs .mymod .hd .hd (this is bad)
11. use a good reset so you don’t have to do margin: 0; more than onece.

give all rules the same strenth
