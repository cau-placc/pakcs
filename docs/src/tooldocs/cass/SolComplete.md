Solution completeness analysis
------------------------------

This analysis assigns to a function a flag which is `True` if this function
is operationally complete, i.e., does not call (explicitly or implicitly)
a rigid function.

For instance, the operation

    not True  = False
    not False = True

is `solution complete`, whereas the prelude operation `putChar`
is not solution complete but may suspend if it is called with
a free variable as argument.
