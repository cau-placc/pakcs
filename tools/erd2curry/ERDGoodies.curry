------------------------------------------------------------------------------
--- This module contains some useful operations on the data types representing
--- entity/relationship diagrams
---
--- @author Michael Hanus
--- @version August 2007
------------------------------------------------------------------------------

module ERDGoodies(erdName,entityName,isEntityNamed,entityAttributes,
                  hasForeignKey,foreignKeyAttributes,
                  attributeName,attributeDomain,hasDefault,
                  isForeignKey,isNullAttribute,
                  cardMinimum,cardMaximum,
                  showERD,combineIds) where

import ERD
import List(intersperse)
import Char(isUpper)
import Maybe

--- The name of an ERD.
erdName :: ERD -> String
erdName (ERD name _ _) = name

--- The name of an entity.
entityName :: Entity -> String
entityName (Entity n _) = n

--- Is this an entity with a given name?
isEntityNamed :: String -> Entity -> Bool
isEntityNamed n e = entityName e == n

--- Has the entity an attribute with a foreign key for a given entity name?
hasForeignKey :: String -> Entity -> Bool
hasForeignKey ename (Entity _ attrs) = any isForeignKeyWithName attrs
 where
  isForeignKeyWithName (Attribute _ d _ _) = case d of KeyDom n -> n==ename
                                                       _        -> False

--- Returns the attributes that are a foreign key of a given entity name.
foreignKeyAttributes :: String -> [Attribute] -> [Attribute]
foreignKeyAttributes ename attrs = filter isForeignKeyWithName attrs
 where
  isForeignKeyWithName (Attribute _ d _ _) = case d of KeyDom n -> n==ename
                                                       _        -> False

--- Returns the names of the foreign key attributes for a given entity name.
foreignKeyAttrNames :: String -> [Attribute] -> [String]
foreignKeyAttrNames ename attrs =
  map attributeName (filter isForeignKeyWithName attrs)
 where
  isForeignKeyWithName (Attribute _ d _ _) = case d of KeyDom n -> n==ename
                                                       _        -> False

--- The attributes of an entity
entityAttributes :: Entity -> [Attribute]
entityAttributes (Entity _ attrs) = attrs

--- The name of an attribute.
attributeName :: Attribute -> String
attributeName (Attribute name _ _ _) = name

--- The domain of an attribute.
attributeDomain :: Attribute -> Domain
attributeDomain (Attribute _ d _ _) = d

--- Has an attribute domain a default value?
hasDefault :: Domain -> Bool
hasDefault (IntDom    d) = isJust d
hasDefault (FloatDom  d) = isJust d
hasDefault (StringDom d) = isJust d
hasDefault (BoolDom   d) = isJust d
hasDefault (DateDom   d) = isJust d
hasDefault (UserDefined _ d) = isJust d

---- Is an attribute a (generated) foreign key?
isForeignKey :: Attribute -> Bool
isForeignKey (Attribute _ d _ _) = case d of KeyDom _ -> True
                                             _        -> False
           
--- Has an attribute a null value?
isNullAttribute :: Attribute -> Bool
isNullAttribute (Attribute _ _ _ isnull) = isnull
           
--- The minimum value of a cardinality.
cardMinimum :: Cardinality -> Int
cardMinimum (Exactly i) = i
cardMinimum (Between i _) = i

--- The maximum value of a cardinality (provided that it is not infinite).
cardMaximum :: Cardinality -> Int
cardMaximum (Exactly i) = i
cardMaximum (Between _ (Max i)) = i


--- A simple pretty printer for ERDs.
showERD :: Int -> ERD -> String
showERD n (ERD en es rs) = "ERD " ++ showString en ++ lb n ++
  " [" ++ concat (intersperse ("," ++ lb (n+2)) (map (showEs (n+2)) es)) ++ "]"
  ++ lb n ++
  " [" ++ concat (intersperse ("," ++ lb (n+2)) (map (showRs (n+2)) rs)) ++ "]"

showEs n (Entity en attrs) = "Entity " ++ showString en ++ lb (n+7) ++
  "[" ++ concat (intersperse ("," ++ lb (n+8)) (map showWOBrackets attrs)) ++"]"

showRs n (Relationship rn ends) =
  "Relationship " ++ showString rn ++ lb (n+13) ++
  "[" ++ concat (intersperse ("," ++ lb (n+14)) (map showWOBrackets ends)) ++"]"

showWOBrackets t = stripBrackets (show t)
 where
  stripBrackets (c:cs) = if c=='(' then reverse (tail (reverse cs)) else c:cs

showString s = "\""++s++"\""

lb n = "\n" ++ take n (repeat ' ')


--- Combines a non-empty list of identifiers into a single identifier.
--- Used in ERD transformation and code generation to create
--- names for combined objects, e.g., relationships and foreign keys.
combineIds :: [String] -> String
combineIds (name:names) = name ++ concatMap maybeAddUnderscore names
 where
  maybeAddUnderscore [] = "_"
  maybeAddUnderscore s@(c:_) = if isUpper c then s else '_' : s
