% Curry Style Guide
% Björn Peemöller, Michael Hanus
% October 06, 2016

This document contains a description of the preferred style
for writing Curry programs.
The described style is oriented to the
[Haskell Style Guide von Johan Tibell][HSG],
but it has been adapted in various points.

In principle, this style guide is a guideline from which
one can deviate _if there are good reasons for it_.

[HSG]: https://github.com/tibbe/haskell-style-guide

# General Formatting

## Line Length

The maximum line length is *80 characters*.

## Indentation

Tabulator stops are not allowed in source programs,
since the actual indentation space of a tabulator stop
might influence the semantics of the program.
Instead, use blanks for indentation. Code blocks should be intended
with *2 spaces*. Most text editors can be configured so that
tabs are automatically replaced by (at most) two spaces.

## Local Definitions

The keyword `where`{.curry} is indented by *one space* and
the subsequent local definitions by a further space.
Hence,  the local definitions are intended, as other code blocks,
by *2 spaces*.
If there is only one short local definition,
it can be written directly after the keyword `where`{.curry}.
Several local value definitions should be aligned at the equality symbol
if they have a comparable number of parameters.
Local definitions should be aligned like top-level definitions
(see below):

~~~ {.curry}
main :: IO ()
main = do
  line <- getLine
  putStrLn $ answer line
 where
  answer s = "Your input: " ++ s

f y = x + y
 where x = 1

g x = x + val1 + secondVal
 where
  val1      = 1
  secondVal = if isZero x then 1 else 2

  -- local operation
  isZero x | x == 0    = True
           | otherwise = False
~~~

## Blank Lines

There should be one blank line between two top-level definitions.
There are no blank lines between type signature and the defining rules.
Similarly, comments of top-level definitions are not separated
from the actual definition with a blank line.

~~~ {.curry}
-- f increments the argument by one
f :: Int -> Int
f x = x + 1

-- fInv decrements the argument by one
fInv x = x - 1
~~~

## Spaces

Binary operators, like `(++)`{.curry}, are surrounded with a single
space on either side.

~~~ {.curry}
-- bad
"Not"++"good"

-- good
"Very" ++ "good"
~~~

It is allowed to omit the surrounding spaces
from simple infix operators, like `(+)`{.curry},
or the infix list constructor `(:)`{.curry}.
However, this is not required.

~~~ {.curry}
f n = n * (n+1) / 2

head (x:_) = x
~~~

Similarly to written texts, a space follows a comma, but there is no space
in front of the comma:

~~~ {.curry}
aList  = [1, 2, 3]
aTuple = (True, "True", 1)
~~~

One _can_ put a space after a lambda, in particular,
if there is non-variable pattern after a lambda.
If there is a variable after a lambda, one can omit the space:

~~~ {.curry}
map (\ (_:_) -> True)
map (\ x -> x + 1)
map (\x -> x + 1)
~~~~

# Formatting of Specific Language Constructs

## Module Header

If the export list fits into one line, it can be written as follows:

~~~ {.curry}
module Set (Set, empty) where
~~~

Longer export lists should be aligned as follows:

~~~ {.curry}
module Data.Set
  ( Set
  , empty
  , singleton
  , member
  ) where
~~~

Optional, one can also put several names into line:

~~~ {.curry}
module Data.Set
  ( Set, empty, singleton
  , member, union, intersect
  ) where
~~~

If there are types exported with constructors,
one should separate the type name and the constructors with a blank:

~~~ {.curry}
module Tree (Tree (..), BinTree (Leaf, Branch)) where
~~~

## Import Declarations

The list of imported modules should be ordered in the following
three categories:

 1. Standard library modules, e.g., `List`{.curry}, `IO`{.curry}
 2. Third party libraries (often not used)
 3. Other local modules (developed with the current application)

The list of imports in each category should be sorted alphabetically.
With the exception of the prelude, all used entities from imported
modules should be explicitly or `qualified`{.curry} imported.
If the list of imported entities is long, one can omit this general rule.

~~~ {.curry}
import           List        (isInfixOf)
import qualified Set  as Set

import SecondParty.Module1 (fun)
import ThirdParty.Module1  (($$$))

import MyUtilsModule -- import everything
~~~

## `data` Declarations

The constructors of a  `data`{.curry} declaration should be
vertically aligned:

~~~ {.curry}
data Tree a =
    Leaf
  | Branch a (Tree a) (Tree a)
~~~

If there are only a few constructors that fits into one line,
one can write the `data`{.curry} declaration as follows:

~~~ {.curry}
data Bit = Zero | One
~~~

Records (`data`{.curry} declarations with named selectors)
should be vertically aligned as follows:

~~~ {.curry}
data Person = Person
    { firstName :: String
    , lastName  :: String
    , age       :: Int
    }
~~~

## Type Signatures

In type signatures one should put a blank before and after
the function arrow `->`{.curry}.

~~~ {.curry}
map :: (a -> b) -> [a] -> [b]
~~~

The type signature should be written in one line if it fits.
If the type signature is long or if one want to put a comment
after the individual types, one should align the function arrows:

~~~ {.curry}
uncurry10 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k)
          -> (a, b, c, d, e, f, g, h, i, j)
          -> k

area :: Int -- width
     -> Int -- height
     -> Int -- area
~~~

## Argument Patterns

If an operation is defined by several rules, one should vertically align
the parameters with the same position:

~~~ {.curry}
and True True = True
and _    _    = False
~~~

The equal signs `=`{.curry} should also be aligned.

## Guards

Guards should immediately follow the patterns or they should be indented
in the following line. In any case, they should be vertically aligned:

~~~ {.curry}
f x y z | x == y    = z
        | otherwise = z + 1

g x y z
  | x == y && not z = 1
  | otherwise       = 0
~~~

If the conditions are very long, the equal signs can start in the next line
below the condition:

~~~ {.curry}
f x y z
  | thisIsAVeryLongConditionWhichNeedsAllTheSpaceAvailableInTheLine x y z
  = 42
  | otherwise
  = 0
~~~

## `if-then-else`

As a general rule, one should try to use patterns and guards instead of
`if-then-else`{.curry} expressions.
Short `if-then-else`{.curry} expressions can be written in one line
(if this fits into the maximal line length):

~~~ {.curry}
f x = g (if x then 0 else 1) 42
~~~

Otherwise, one should indent the `then`{.curry} and `else`{.curry}
branch, which are vertically aligned:

~~~ {.curry}
foo = if ...
        then ...
        else ...
~~~

If the condition is short, one can also write the `if`{.curry} and
`then`{.curry} part in one line, but align the
`then`{.curry} and `else`{.curry} parts:

~~~ {.curry}
foo = if ... then ...
             else ...
~~~

## `case` Expressions

The alternatives in a `case`{.curry} expression should be aligned in the
following form:

~~~ {.curry}
foobar = case something of
  Just j  -> foo
  Nothing -> bar
~~~

or

~~~ {.curry}
foobar = case something of
           Just j  -> foo
           Nothing -> bar
~~~

The arrows `->`{.curry} should be aligned to improve readability.


## `let` Expressions

Short `let`{.curry} expressions with a single local declaration
can be written in one line (if the line is not too long):

~~~ {.curry}
squareSum x y = let z = x + y in z * z

grandfather g c | let f free in father g f && father f c = True
~~~

Long `let`{.curry} expressions or `let`{.curry} expressions
with more then one local declaration should be aligned so that
the keywords `let`{.curry} and `in`{.curry}
are in the same column:

~~~ {.curry}
qsort (x:xs) = let (l,r) = split x xs
               in qsort l ++ (x:qsort r)

doubleSquareSum x y =
  let z  = x + y
      sq = z * z
  in sq + sq
~~~

The equal signs of all local declarations should be vertically aligned.
The same rule is used for `let`{.curry} expressions in
`do`{.curry} blocks (where there is no `in`{.curry} expression).


## `do` Blocks

The statements in a `do`{.curry} blocks should start immediately
after the keyword `do`{.curry} or they are indented in the next line:

~~~ {.curry}
echo = do name <- getLine
          putStrLn name

greet = do
  putStr "How is your name? "
  name <- getLine
  putStrLn ("Hello " ++ name ++ "!")
~~~

## List/Tuple Definitions

The element of long lists should be aligned as follows:

~~~ {.curry}
exceptions =
  [ InvalidStatusCode
  , MissingContentHeader
  , InternalServerError
  ]
~~~

One can also omit the first line break:

~~~ {.curry}
directions = [ North
             , East
             , South
             , West
             ]
~~~

Short lists can be written in one line:

~~~ {.curry}
short = [1, 2, 3]
~~~

The same rules apply to tuple definitions:

~~~ {.curry}
t       = (1, True)
ignored = ( InvalidStatusCode
          , MissingContentHeader
          )
~~~

## Brackets

Superfluous brackets should be avoided:

~~~ {.curry}
seven = 1 + 2 * 3

f x = if cond x then 0 else 1
~~~

instead of

~~~ {.curry}
seven = (1 + (2 * 3))

f x = if (cond x) then 0 else 1
~~~

In case of specific infix operators (e.g., not defined in the prelude)
or if one is not sure about the precedence or want to document
the predence, one can write brackets.


# Comments

## Language

Comments should be written in correct English.
The identifiers used in a program should be also meaningful
in English.


## Top-Level Declarations

All top-level operations should have a comment and a type signature.
This is a must for exported operations.
One should use the syntax of [CurryDoc] for the comments of
exported operations so that the program documentation can easily be generated.

[CurryDoc]: https://www-ps.informatik.uni-kiel.de/currywiki/tools/currydoc

~~~{.curry}
--- Splits the list argument into a list of lists of related adjacent
--- elements.
--- @param eq - the relation to classify adjacent elements
--- @param xs - the list of elements
--- @return the list of lists of related adjacent elements
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
~~~

The comment of an operation should contain enough information so that
one can use the operation without studying its implementation.

Data types should be commented in a similar way.
Exported constructors should have individual comments:

~~~ {.curry}
--- The data type for representing XML expressions.
--- @cons XText - a text string (PCDATA)
--- @cons XElem - an XML element with tag field, attributes, and a list
---               of XML elements as contents
data XmlExp = XText String
            | XElem String [(String,String)] [XmlExp]
~~~

For records with explicit selectors (labels), one should add a comment
for each selector:

~~~ {.curry}
--- A natural person
--- @field firstName - First name, may contain several ones
--- @field lastName  - Last name
--- @field height    - Height in centimeters
data Person =
    Person
      { firstName :: String
      , lastName  :: String
      , height    :: Int
      }
~~~

## End-of-Line Comments

Comments at the end of a line should be separated from the code
by at least one space:

~~~{.curry}
foo :: Int -> Int
foo n = salt * 32 + 9
  where salt = 453645243  -- Magic hash salt.
~~~

# Naming

Use camel case for names that consists of several logical units (words):

~~~ {.curry}
data BankAccount = ...

thisIsTheAnswer = 42
~~~

To improve readability, one should not capitalize all letters
in abbreviations.
For instance, one should write
`showXmlDoc`{.curry} instead of `showXMLDoc`{.curry}.
An exception are abbreviations with only two letters,
like `IO`{.curry}.

Parameter names and names of local declarations are usually short,
but one has to take the following rule into account:

> Entity with larger visibility regions have longer names.

The name `x`{.curry} can be used in a one-line operation but never
as a top-level declaration.

One should also take into account the following
*conventions for short names*:

  * `f`{.curry} denotes an operation of type  `a -> b`{.curry}

  * `n`{.curry}, `m`{.curry}, etc. denote natural numbers of type `Int`{.curry}

  * `x`{.curry}, `y`{.curry}, `z`{.curry} often denote values of
    polymorphic types

  * A name with suffix `s`{.curry} denote several values like lists.
    Here are some examples:
    `xs :: [a]`{.curry}, `fs :: [a -> b]`{.curry},
    `xss :: [[a]]`{.curry}

  * `p`{.curry} denotes a predicate of type `a -> Bool`{.curry}

  * A name with an apostroph, like  `x'`{.curry}, denote modified values:

    ~~~{.curry}
    let acc' = updateAcc acc in ...
    ~~~

  * `k`{.curry} and `v`{.curry} are often used for *keys* and *values*
    in a mapping.


# Compiler Warnings

Code should be compilable with the parsing option

~~~{.curry}
:set parser -Wall
~~~

without producing any warnings.
If one uses a more logic-oriented programming style,
operations are often defined with overlapping rules or
incomplete pattern matching.
In this case, one can explicitly omit the warnings by the parsing option

~~~{.curry}
{-# OPTIONS_CYMAKE -Wno-incomplete-patterns -Wno-overlapping #-}
~~~

in the beginning module header.
However, this should be done *only* in a program that depends on
these features of Curry.
