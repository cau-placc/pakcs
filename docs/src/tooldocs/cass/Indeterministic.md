Indeterminism analysis
----------------------

This analysis assigns to each operation a flag which is `True`
if this operation might be indeterministic, i.e., calls directly or indirectly
a select or committed choice operation.
Thus, an indeterministic is not referentially transparent
since it might deliver different results on different program runs.
