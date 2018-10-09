Residuation analysis
--------------------

This analysis checks whether a function does not residuate and yields,
if it successfully evaluates to some value,
a ground value provided that the function is called with some
ground values as arguments.

To bemore precise, the analysis associates to each function
one of the following results:

* `NoResiduateIf xs` (does not residuate if arguments `xs` are ground):

  If the operation is called where the arguments in the index list `xs`
  are ground values (where arguments are numbered from 1),
  then the evaluation does not residuate and yields a ground value.
  For instance, the operation

    const :: a -> b -> a
    const x _ = x

  has the residuation behavior `NoResiduateIf [1]`, and the
  list concatenation `++` has the residuation behavior `NoResiduateIf [1,2]`.

* `MayResiduate` (possible residuation or non-ground result):

  The operation might residuate or yields a non-ground value,
  independent of the arguments. For instance, this is the case
  for the operations

    f x = x + ensureNotFree unknown

    g x = (x,y)  where y free

* `NoResInfo` (unknown residuation behavior):

  The residuation behavior of this function cannot be determined.
  This might occur when complex recursive `let`s are involved.
