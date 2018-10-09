Sibling constructor analysis
----------------------------

This analysis associates to each data constructor the list of
sibling constructors, i.e., the qualified name and arity of
all constructors of the same type without this data constructor.

For instance, the sibling constructors of `Prelude.True` are
`[(("Prelude","False"),0)]`.
