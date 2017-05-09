Possible Extensions
===================

- Improve partial evaluation of addition with the following rules:

  ~~~
  l1 + (l2 + e) -> (l1 +A l2) + e
  l1 + (e + l2) -> (l1 +A l2) + e
  (l1 + e) + l2 -> e + (l1 +A l2)
  (e + l1) + l2 -> e + (l1 +A l2)
  ~~~

- Extend implementation to also consider function declarations
  of imported modules

- Integrate algebraic optimizations such as, e.g, map-fusion:
  `map f (map g) xs => map (f . g) xs`.
  A concrete example for its usefulness is in `test/papers/wflp/scanl`.

- Implement let-rewriting semantics: Derive an operational semantics
  based from its denotational semantics and implement it

- Think about a generalization that may introduce new higher-order arguments
  if some substitution contains locally bound variables.

- Investigate the problems resulting of the partial evaluation functional
  patterns constructing bindings of increasing size, e.g.,
  `test/funpats/list/init`.
  Presumably this requires the following steps:
  - Investigate the example by hand and try to find a implementation
    that does not use functional patterns but has exactly the same semantics.
  - Think about the necessary transformation steps and whether they can be
    incorporated into the partial evaluator.
  - Often, this will require additional techniques such as strictness
    analysis to enable certain transformation steps, I suppose.

Known Limitations
=================

- Functions that use polymorphic recursion, which is an extension on
  standard Curry on its own, can currently be partially evaluated without
  further problems, but the subsequent type inference fails since the
  type annotations have been discarded.
  An example for this limitation can be found in `newtests/PolyRec.curry`.
