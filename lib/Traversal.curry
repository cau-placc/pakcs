---------------------------------------------------------------------------
--- Library to support lightweight generic traversals
--- through tree-structured data.
--- See <a href="http://www-ps.informatik.uni-kiel.de/~sebf/projects/traversal.html">here</a>
--- for a description of the library.
---
--- @author Sebastian Fischer
--- @version February 2008
---------------------------------------------------------------------------

module Traversal (

  Traversable, noChildren,

  children, replaceChildren, mapChildren,

  family, childFamilies, mapFamily, mapChildFamilies,

  evalFamily, evalChildFamilies, fold, foldChildren,

  replaceChildrenIO, mapChildrenIO, mapFamilyIO, mapChildFamiliesIO,

  evalFamilyIO, evalChildFamiliesIO

  ) where

--- A datatype is <code>Traversable</code> if it defines a function
--- that can decompose a value into a list of children of the same type
--- and recombine new children to a new value of the original type. 
---
type Traversable a b = a -> ([b], [b] -> a)

--- Traversal function for constructors without children.
---
noChildren :: Traversable _ _
noChildren x = ([], const x)

--- Yields the children of a value.
---
children :: Traversable a b -> a -> [b]
children tr = fst . tr

--- Replaces the children of a value.
--- 
replaceChildren :: Traversable a b -> a -> [b] -> a
replaceChildren tr = snd . tr

--- Applies the given function to each child of a value.
---
mapChildren :: Traversable a b -> (b -> b) -> a -> a
mapChildren tr f x = replaceChildren tr x (map f (children tr x))

--- Computes a list of the given value, its children, those children, etc.
---
family :: Traversable a a -> a -> [a]
family tr x = familyFL tr x []

--- Computes a list of family members of the children of a value.
--- The value and its children can have different types.
---
childFamilies :: Traversable a b -> Traversable b b -> a -> [b]
childFamilies tra trb x = childFamiliesFL tra trb x [] 

--- Applies the given function to each member of the family of a value.
--- Proceeds bottom-up.
---
mapFamily :: Traversable a a -> (a -> a) -> a -> a
mapFamily tr f = f . mapChildFamilies tr tr f

--- Applies the given function to each member of the families of the children
--- of a value. The value and its children can have different types.
--- Proceeds bottom-up.
---
mapChildFamilies :: Traversable a b -> Traversable b b -> (b -> b) -> a -> a
mapChildFamilies tra trb = mapChildren tra . mapFamily trb

--- Applies the given function to each member of the family of a value 
--- as long as possible. On each member of the family of the result the given
--- function will yield <code>Nothing</code>.
--- Proceeds bottom-up.
---
evalFamily :: Traversable a a -> (a -> Maybe a) -> a -> a
evalFamily tr f = mapFamily tr g
 where g x = maybe x (mapFamily tr g) (f x)

--- Applies the given function to each member of the families of the children
--- of a value as long as possible.
--- Similar to 'evalFamily'.
---
evalChildFamilies :: Traversable a b -> Traversable b b
                  -> (b -> Maybe b) -> a -> a
evalChildFamilies tra trb = mapChildren tra . evalFamily trb

--- Implements a traversal similar to a fold with possible default cases.
---
fold :: Traversable a a -> (a -> [r] -> r) -> a -> r
fold tr f = foldChildren tr tr f f

--- Fold the children and combine the results.
---
foldChildren :: Traversable a b -> Traversable b b
             -> (a -> [rb] -> ra) -> (b -> [rb] -> rb) -> a -> ra
foldChildren tra trb f g a = f a (map (fold trb g) (children tra a))

--- IO version of replaceChildren
---
replaceChildrenIO :: Traversable a b -> a -> IO [b] -> IO a
replaceChildrenIO tr = liftIO . replaceChildren tr

--- IO version of mapChildren
---
mapChildrenIO :: Traversable a b -> (b -> IO b) -> a -> IO a
mapChildrenIO tr f a = replaceChildrenIO tr a (mapIO f (children tr a))

--- IO version of mapFamily
---
mapFamilyIO :: Traversable a a -> (a -> IO a) -> a -> IO a
mapFamilyIO tr f a = mapChildFamiliesIO tr tr f a >>= f

--- IO version of mapChildFamilies
---
mapChildFamiliesIO :: Traversable a b -> Traversable b b
                    -> (b -> IO b) -> a -> IO a
mapChildFamiliesIO tra trb = mapChildrenIO tra . mapFamilyIO trb

--- IO version of evalFamily
---
evalFamilyIO :: Traversable a a -> (a -> IO (Maybe a)) -> a -> IO a
evalFamilyIO tr f = mapFamilyIO tr g
 where g a = f a >>= maybe (return a) (mapFamilyIO tr g)

--- IO version of evalChildFamilies
---
evalChildFamiliesIO :: Traversable a b -> Traversable b b
                    -> (b -> IO (Maybe b)) -> a -> IO a
evalChildFamiliesIO tra trb = mapChildrenIO tra . evalFamilyIO trb


-- implementation of 'family' with functional lists for efficiency reasons

type FunList a = [a] -> [a]

concatFL :: [FunList a] -> FunList a
concatFL [] ys = ys
concatFL (x:xs) ys = x (concatFL xs ys)

familyFL :: Traversable a a -> a -> FunList a
familyFL tr x xs = x : childFamiliesFL tr tr x xs

childFamiliesFL :: Traversable a b -> Traversable b b -> a -> FunList b
childFamiliesFL tra trb x xs = concatFL (map (familyFL trb) (children tra x)) xs

liftIO :: (a -> b) -> IO a -> IO b
liftIO f ioa = ioa >>= return . f

