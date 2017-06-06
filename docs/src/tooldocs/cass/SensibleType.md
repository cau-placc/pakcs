Sensible types analysis
-----------------------

The `SensibleType` analysis is a type analysis which checks
whether a type is sensible, i.e., whether there exists at least
one value of this type. This analysis associates to each type
constructor the following information:

* sensible, i.e., there is exists some value of this type
* parametric sensible, i.e., it is parametric type which is sensible
  if all type arguments are instantiated with sensible types
* not sensible, i.e., there may be no values of this type

For instance, the list type constructor "[]" is sensible
and the pair type constructor "(,)" is parametric sensible.
For further examples, consider the following type declarations:

    type Pair = (Int,Int)
    
    data RTree a = RTree a [RTree a]
    
    data ITree a = ITree a (ITree a)
    
    type IntRTree = RTree Int
    
    type IntITree = ITree Int
    
    type ITreeRTree = RTree (ITree Int)

Then this analysis computes the following information:

    Pair       : sensible
    RTree      : parametric sensible
    ITree      : not sensible
    IntRTree   : sensible
    IntITree   : not sensible
    ITreeRTree : not sensible

Note that function types are classified as not sensible since it is
not known whether some operation of this type exists.
