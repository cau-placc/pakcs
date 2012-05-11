------------------------------------------------------------------------------
--- Library with some useful functions on the Maybe datatype
---
--- @author Frank Huch (source from the corresponding Hugs module)
---         Bernd Brassel (sequence, mapM)
--- @version April 2005
------------------------------------------------------------------------------

module Maybe(
    isJust, isNothing,
    fromJust, fromMaybe, listToMaybe, maybeToList,
    catMaybes, mapMaybe,(>>-), sequenceMaybe, mapMMaybe,

    -- ... and what the Prelude exports
    Maybe(..),
    maybe
    ) where

infixl 1 >>-

isJust :: Maybe _ -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe _ -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing  = error "Maybe.fromJust: Nothing"

fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing  = d
fromMaybe _ (Just a) = a

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (a:_) = Just a
 
catMaybes :: [Maybe a] -> [a]
catMaybes ms = [ m | (Just m) <- ms ]

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = catMaybes . map f

--- Monadic bind for Maybe.
--- Maybe can be interpreted as a monad where Nothing is interpreted
--- as the error case by this monadic binding.
--- @param maybeValue - Nothing or Just x
--- @param f - function to be applied to x
--- @return Nothing if maybeValue is Nothing,
---         otherwise f is applied to x
(>>-) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>- _ = Nothing
(Just x) >>- f  = f x

--- monadic sequence for maybe
sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe [] = Just []
sequenceMaybe (c:cs) = c >>- \x -> sequenceMaybe cs >>- \xs -> Just (x:xs)

--- monadic map for maybe
mapMMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMMaybe f = sequenceMaybe . map f


-----------------------------------------------------------------------------
