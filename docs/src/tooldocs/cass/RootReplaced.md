Root replacement analysis
-------------------------

This analysis returns for each function `f` all functions into which `f` can
be replaced at the root. For instance, if there are the definitions:

    f x = g x
    
    g x = h x
    
    h x = k x : []

    k x = x

then the root replacements of `f` are `[g,h]` and the
root replacements of `g` are `[h]`.

This analysis could be useful to detect simple loops, e.g., if
a function is in its own root replacement.
