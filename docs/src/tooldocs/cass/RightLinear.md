Right-linearity analysis
------------------------

This analysis checks whether an operation is right-linear, i.e.,
whether its evaluation does not duplicate any argument.
Hence, this analysis returns `right-linear` for a given operation
if it is defined by right-linear rules (i.e., rules that does not
contain multiple occurrences of argument variables in its right-hand sides)
and depends only on right-linear operations.

