------------------------------------------------------------------------------
--- This module provides a transformation on ERD terms that eliminates
--- the original relationships by introducing new entities for complex
--- relationships and inserting foreign keys for simple relationships.
------------------------------------------------------------------------------

module Transformation(transform) where

import ERD 
import ERDGoodies

---------------------------------------------------
-- The main transformation which adds
-- - artificial keys of type Int for all entities (not null)
--   user defined keys -> unique
-- - foreign keys for 1:1 and 1:n relationships
-- - new entities and relationships for n:m relationships (delete old relationship) 

transform :: ERD -> ERD
transform (ERD name entities relationships) = 
  let (es,rs) = transformRel (map addKey entities,[]) relationships 
  in ERD name es rs
   
-- Adds a new artificial primary key to an entity
addKey :: Entity -> Entity  
addKey (Entity en attrs) =
  Entity en ((Attribute ("Key") (IntDom Nothing) PKey False) 
             : (map deleteKey attrs))
 where
  -- set user-defined primary keys to "Unique"
  deleteKey :: Attribute -> Attribute
  deleteKey a@(Attribute an d k null)
    | k == PKey = Attribute an d Unique null
    | otherwise = a


transformRel :: ([Entity], [Relationship]) -> [Relationship]
             -> ([Entity], [Relationship])
transformRel (ens,rels) [] = (ens,rels)
transformRel (ens,rels) (r@(Relationship _ [(REnd e1 _ c1), (REnd e2 _ c2)]) : rs) = case c1 of
  (Exactly i1)         -> case c2 of 
    (Exactly _)           -> error "2 minima" --error in XML2ERD
    (Between _ Infinite)  -> transformRel (eRN i1 e1 e2 (ens, rels) r) rs
    (Between i2 (Max i3)) -> transformRel (eRJ i1 i2 i3 e1 e2 (ens, rels) r) rs
    
  (Between i1 Infinite)   -> case c2 of 
    (Exactly i2)          -> transformRel (eRN i2 e2 e1 (ens, rels) r) rs
    (Between _ Infinite)  -> transformRel (rNRN ens rels r) rs
    (Between i2 (Max i3)) -> transformRel (rNRJ i1 i2 i3 e1 e2 ens rels r) rs

  (Between i1 (Max i2)) -> case c2 of 
    (Exactly i3)          -> transformRel (eRJ i3 i1 i2 e2 e1 (ens, rels) r) rs 
    (Between i3 Infinite) -> transformRel (rNRJ i3 i1 i2 e1 e2 ens rels r) rs
    (Between i3 (Max i4)) -> transformRel (rJRJ i1 i2 i3 i4 e1 e2 ens rels r) rs


eRN :: Int -> String -> String -> ([Entity],[Relationship]) -> Relationship 
    -> ([Entity],[Relationship])
eRN i1 e1 e2 (ens,rels) r@(Relationship rname _) 
  | i1==1 = (addFKey e1 e2 rname False False ens ens, (r:rels)) --(1,1):(0,n)
  | otherwise = let (r1,e,r2) = addExtraEntity r ens            --(i,i):(0,n)
                in 
                (e:ens, r1:r2:rels)

eRJ :: Int -> Int -> Int -> String -> String -> ([Entity],[Relationship]) -> Relationship 
    -> ([Entity],[Relationship])
eRJ i1 _ i3 e1 e2 (ens, rels) r@(Relationship rname _)
  | i1==1 = (addFKey e1 e2 rname False (i3==1) ens ens, (r:rels))       --(1,1):(0,j)
  | otherwise = if i3==1   
                then (addFKey e2 e1 rname True False ens ens, (r:rels)) --(i,i):(0,1) 
                else let (r1,e,r2) = addExtraEntity r ens               --(i,i):(0,j)
                     in 
                     (e:ens, r1:r2:rels)                                       

rNRN :: [Entity] -> [Relationship] -> Relationship -> ([Entity],[Relationship])
rNRN ens rels r =
  let (r1,e,r2) = addExtraEntity r ens
  in
  (e:ens, r1:r2:rels)

rNRJ _ i2 i3 e1 e2 ens rels r@(Relationship rname _)  
  | i2==0 && i3==1 = (addFKey e1 e2 rname True False ens ens, (r:rels)) --(_,n):(0,1)
  | otherwise = let (r1,e,r2) = addExtraEntity r ens                    --(_,n):(_,i)
                in 
                (e:ens, r1:r2:rels) 

rJRJ i1 i2 _ i4 e1 e2 ens rels r@(Relationship rname _) 
  | i1==0 && i2==1 = (addFKey e1 e2 rname True (i4==1) ens ens, (r:rels)) --(0,1):(0,1)/(0,1):(_,j)
  | otherwise = let (r1,e,r2) = addExtraEntity r ens                      --(_,i):(_,j)
                in 
                (e:ens, r1:r2:rels) 


addFKey :: String -> String -> String -> Bool -> Bool -> [Entity] -> [Entity] -> [Entity]
addFKey _ _ _ _ _ [] ens = ens
addFKey e1 e2 rname null unique (e@(Entity n (a:attrs)) : ens) ens'
  | e2 == n = 
     let aname = getAttributeName (getKeyAttribute e1 ens')   
     in 
     (Entity n
             (a:attrs++[Attribute (fKeyName e1 rname aname) (KeyDom e1) 
                                  (if unique then Unique else NoKey) null]))
      : ens 
  | otherwise = e : addFKey e1 e2 rname null unique ens ens' 


--foreign key for extra entity
addFKey' :: String -> String -> Bool -> Entity -> [Entity] -> Entity
addFKey' ename rname null (Entity n attrs) es = 
 Entity n 
        ((Attribute (fKeyName ename rname
                              (getAttributeName (getKeyAttribute ename es))) 
                    (KeyDom ename) PKey null) : attrs)

getAttributeName :: Attribute -> String
getAttributeName (Attribute n _ _ _) = n
getKeyAttribute :: String -> [Entity] -> Attribute
getKeyAttribute ename ((Entity n attrs) : ens) 
  | ename == n = getKey attrs
  | otherwise = getKeyAttribute ename ens
getKey :: [Attribute] -> Attribute
getKey (a@(Attribute _ _ k _):attrs)
  | PKey == k = a
  | otherwise = getKey attrs  

-- e1   -   e2
-- e1 - R - e2
addExtraEntity :: Relationship -> [Entity]
                  -> (Relationship, Entity, Relationship)
addExtraEntity (Relationship r [(REnd e1 r1 c1), (REnd e2 r2 c2)]) es =
  (Relationship "" [(REnd e1 "" (Exactly 1)), (REnd r r2 c2)],
   addFKey' e1 r False (addFKey' e2 r False (Entity r []) es) es,
   Relationship "" [(REnd e2 "" (Exactly 1)), (REnd r r1 c1)])

-- create a name for a foreign key for a given entity, relationship,
-- and key attribute name.
fKeyName :: String -> String -> String -> String
fKeyName ename rname kname = combineIds [ename,rname,kname]

