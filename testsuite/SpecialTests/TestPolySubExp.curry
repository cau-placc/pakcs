------------------------------------------------------------------------------
--- Some tests for locally polymorphic sub-expressions, i.e., polymorphic
--- sub-expressions where the polymorphism is not visible in the type of the
--- enclosing expression.
--- This led to compilation errors with an earlier version of KiCS2, and is
--- fixed in commit:931bffe5085e5355cd7659f98d704f7c2932d2e6.
---
--- To run all tests automatically by the currytest tool, use the command:
--- "curry check testPolySubExp"
---
--- @author Björn Peemöller
--- @version July 2013
------------------------------------------------------------------------------

import Test.Prop

poly1 = fst (3, id)

testPolymorphicId = poly1 -=- 3

poly2 = null []

testNullOfEmptyList = always poly2

poly3 = case [] of
          _ -> 42

testMatchingEmptyList = poly3 -=- 42
