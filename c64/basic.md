# Commodore 64 basic

The default numerical type is a float

    Z = 12
    XX = 34.6

String type ($ suffix)

    Z$ = "hello" + " " + "world"

Integer type (% suffix) (actually slower than floats)

    X% = 24 + 5

convert string value to numerical

    Z = VAL("123.4")

convert numerical value to string

    Z$ = STR$(123.4)

print values to screen (? is shorthand)

    PRINT Z
    ? Z

: functions a bit like ; in C languages - lets us put multiple statements on one line
max line length is 80 chars

    A = 12 : B = 14 : PRINT A B
