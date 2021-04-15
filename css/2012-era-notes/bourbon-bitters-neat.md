# bourbon

  * provides a library of _mixins_
  * bourbon adds no output unless you invoke one of its functions|mixins|variables
  * I feel like I won't use a whole lot of what bourbon provides

# bitters
bitters is a starter template for the project
bitters is designed to be copied in and then modified
bitters is just an example app for bourgon & neat
  * I am confused about how extends and mixins fit together in bitters
it is a bit unnecesasry how they use mixins and extends together for uber flexibility

# neat
neat is a collection of mixins
    * does not generate any output on its own
    * neat-helpers includes the neat functions and grid settings

mixins dir should just contain mixin definitions
mixins can be used by any styles in our app

extends are styles that exist solely to be included in other styles
they can be used by elements or modules or states

Q: what is the advantage of extends over mixins?

extends generate smaller css
- extends make the generated css less
- mixins make larger css
= both create a dependency between client selector and the mixin|extend
? using both in the same project a bad idea?
