Termination analysis
--------------------

This analysis assigns `True` to an operation `f` if all its evaluations
on ground argument terms are finite.

The current method used in this analysis is quite simple.
It checks whether the arguments in all recursive
calls of an operation are smaller than the arguments passed to
the operation. Indirect calls are not considered.
Therefore, the operation

    length []     = 0
    length (x:xs) = 1 + length xs

is classified as terminating, whereas the semantically equivalent operation

    length []     = 0
    length (x:xs) = incLength xs
    
    incLength xs = 1 + length xs

is classified as possibly non-terminating.

Operations containing free variables in their definitions are
also classified as possibly non-terminating since a free variable
might reduce to arbitrarily large constructor terms (in case of
recursive data types).
