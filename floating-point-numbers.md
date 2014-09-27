# Sources

* http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
* http://www.cprogramming.com/tutorial/floating_point/understanding_floating_point_representation.html
* http://en.wikipedia.org/wiki/Floating_point

# Terminology

Significant figures of a number are those figures that carry meaning contributing to its precision i.e.

Significant figures are all digits except:

1. leading zeros
    0.00052 = 2 significant figures: 5, 2
    0000054 = 2 significant figures: 5, 4
2. trailing zeros _only_ when they are just placeholders to indicate scale
    even this is a bit ambigious
    13400 = probably 5 significant digits (sometimes bar over last signficant
    digit or sometimes an explicit decimal point is used e.g.  13400.
    12.334000 = 8 significant digits (trailing 0 is significant if num has decimal point!!!)
3. spurious digits resulting from
    1. measurements reported to a greater precision than the equipment supports
    2. calculations carried out to greater precision than the source data

Significance arithmetic is

* a simplified _propagation of uncertainty rules_
* a rough set of rules for maintaining significance throughout a computation


Scientific notation;

* removes any ambiguity about trailing 0 being significant
* all digits are signifcant (otherwise they shouldn't be there)
* part of the representation that contains significant figures is called the _significand_

    1.3400 * 10^4
    1300 = ambigious about whether the trailing 0s are significant
    1.300 * 10^3 = 1300 to 4 significant figures
    1.3 * 10^3 = 1300 to two significant figures

# Representing a wide range of numbers

In computing, floating point describes a method of representing an approximation
of a real number in a way that can support a wide range of values.

The problem it solves is representing very large and very small numbers at the
same time

```
significant digits * base^exponent
```

Contrast with integers which only have significand.

Floating point numbers trade greater *range* for less *precision*

http://floating-point-gui.de/formats/fp/

When you want to store a fractional number in a computer you have only so many
digits available .e.g. 64 bit word size

You need to allocate some digit to representing the integer part and allocate
the rest to the fractional part. This is true whether you are using bits (base
2) or decimal digits (base 10)

3 digits for integer, 4 digits for fractional
023.4569

If you choose a fixed allocation it is difficult to store very small numbers and
very large nmbers e.g.

0.9937474894020 # lots of fractional digits, one integer digit
230000834587490504.1 # many integer digits, one franctional digit

To solve this we use a "floating point" number

we store

1. the numbers digits (the _significand_)
2. where the decimal point should be placed relative to the start of the significand (the _exponent_)

In decimal system, floating point numbers usually take the form of scientific
notation


# IEEE 754 representation

significand is also called _mantissa_

To prevent having many bit strings that could represent the same number e.g.

2 * 10^-1
0.2 * 10^0
20 * 10^-2

### 1.m mantissa representation

we interpret the mantissa (significand) bits as all being to the right of the
point with an assumed (binary) 1 on the left of the point. This is called `1.m`
representation.

Any binary number that isn't 0 has a 1 somewhere so we can always make `1.m`
representation work by choosing the exponent so the left-most 1 in the binary
number is on the LHS of the point.

Basically the significands most significant bit is assumed to be 1 and is not included

### shift-127 exponent representation

1.m representation has no way to represent decimal `1.0` i.e. in `1.m` because
of the implied 1 on LHS it would be represented by all 0 mantissa and all 0
exponent which is also the representation of 0

To get around this they use a formula to get from the stored exponent to the
actual:

```
actual exponent = exponent from bit string - 127
```

The downside of this is that instead of being able to represent 2^-127, the
smallest number we can represent is 2^126 but this is a better trade off than
not being able to tell the difference between `1.0` and `0`!

## The bit string

The bits appear in this order:

single precision
    1 sign bit
    8 exponent bits
    23 significand bits
32 bits total

double precision
    1 sign bit
    11 exponent bits
    52 significand bits
64 bits total

exponent bits
    it does not have a sign - uses a custom scheme to represent sign


## Special cases

### 0

* If all bits are 0 (sign bit is ignored) then the value is 0
* There are separate positive and negative 0 patterns (all bits 0 except the sign bit)

### Infinity

* exponent all 1, significand all 0 = representation of Infinity
* there is positive/negative infinity depending on sign
* positive Infinity and negative Infinity must be considered equal

### NaN

* exponent all 1, significand *not* all 0 = representioan of NaN
* represents the result of various undefined calculations
* there is positive and negative NaN
* _Even bit identical NaN values must *not* be considered equal_

### other

there is another special case where exponent bits are 0 and the mantissa is
interpreted as a simple bit string - more at
http://www.cprogramming.com/tutorial/floating_point/understanding_floating_point_representation.html

### Loss of precision

Because floating point numbers have limited precision, doing operations on them
with other floats can cause your answers to 'degrade' over time (as the errors
multiply)

Possible solution:

If you are dealing with fractions and want to preserve accuracy, store them as
two integers and only make the floating point representation when you need it.


# Ruby and floats

Can make a NaN value in ruby by `Float::NAN`

Can test if an instance of Float is NaN via `#nan?` - you will get a NoMethodError if you try it on other types

```
>> x = 23
23
>> x.nan?
NoMethodError: undefined method `nan?' for 23:Fixnum
	from (irb):17
	from /Users/eoinkelly/.rbenv/versions/2.1.2/bin/irb:11:in `<main>'
>> x = 23.0
23.0
>> x.nan?
false
>> x = 23 / 0.0
Infinity
>> x.nan?
false
>> x = 0 / 0.0
NaN
>> x.nan?
true
```

Dividing by 0 (instance of Fixnum) raises a ZeroDivisionError **but** dividing by `0.0` (instance of Float) returns `NaN`!!!

```
>> 0 / 0
ZeroDivisionError: divided by 0
	from (irb):9:in `/'
	from (irb):9
	from /Users/eoinkelly/.rbenv/versions/2.1.2/bin/irb:11:in `<main>'
>> 0.0 / 0
NaN
>> 0 / 0.0
NaN
```

## Converting a string to float in Ruby

Use Float(str) as it is safer - it will raise an arguemnt error if it can't
convert the whole string whereas `str.to_f` will just return `0.0`

```
>> "0.34".to_f
0.34
>> "0.34hi".to_f
0.34
>> "h0.34hi".to_f
0.0
>> Float("0.34")
0.34
>> Float("0.34hi")
ArgumentError: invalid value for Float(): "0.34hi"
	from (irb):36:in `Float'
	from (irb):36
	from /Users/eoinkelly/.rbenv/versions/2.1.2/bin/irb:11:in `<main>'
```
