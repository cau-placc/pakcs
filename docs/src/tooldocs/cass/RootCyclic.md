Root cyclic analysis
--------------------

This analysis assigns `True` to an operation `f` if its evaluation
might result in an expression rooted by `f`.
Hence, this analysis is useful to detect simple loops.


    f x = g x
    
    g x = h x
    
    h x = id (f x)

    id x = x

Then `f`, `g`, and `h` are root-cyclic whereas `id` is not root-cyclic.
