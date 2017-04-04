Analyzing types occurring in values
-----------------------------------

The `TypesInValues` analysis is a type analysis which assigns
to each data type defined in a program the list of data types
(type constructors) which might occur in value arguments of this type.

For instance, no type constructors are associated to `Bool`
since Boolean values have no arguments.
The type constructor `[]` is associated to the `[]` since a list
occurs in the second argument of a non-empty list.

Thus, this analysis can be used to check for recursive types:
if a type constructor is associated to itself, the type is recursive,
i.e., can have values of arbitrary size.

For instance, consider the following type declarations:

    data List a = Empty | Cons a (List a)
    
    data Tree a = Leaf | Node (List (Tree a))

Then this analysis computes the following information:

    List : List
    Tree : List, Tree

Hence, both types are recursive.
