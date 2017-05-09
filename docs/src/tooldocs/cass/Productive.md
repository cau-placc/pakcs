Productivity analysis
---------------------

This analysis computes some information about the termination
or productivity of an operation.

An operation is considered as being productive if it cannot
perform an infinite number of steps without producing
outermost constructors.

This analysis assigns to an operation an abstract value
indicating whether the function is terminating, looping, or productive.
In the latter case, the abstract value contains the top-level calls,
i.e., operations that are called at the top-level without an outermost
constructor.

For instance, consider the operations

    loop = id loop

    ones = 1 : ones

`loop` is classified as looping whereas `ones` is productive.
