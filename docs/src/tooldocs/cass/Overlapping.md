Overlapping rule analysis
-------------------------

The overlapping rule analysis checks whether an individual operation
is defined by overlapping left-hand sides.

For instance, the operation

    not True  = False
    not False = True

is not overlapping, whereas

    coin = 0
    coin = 1

is overlapping. Note that

    f = coin

is not overlapping, although it calls an operation defined by
overlapping rules.
