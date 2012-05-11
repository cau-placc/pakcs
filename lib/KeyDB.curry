--- This module provides a general interface for databases
--- (persistent predicates) where each entry consists of a key and an info
--- part. The key is an integer and the info is arbitrary.
--- All functions are parameterized with a dynamic predicate that
--- takes an integer key as a first parameter.
---
--- Remark:
--- This library has been revised to the library <code>KeyDatabase</code>.
--- Thus, it might not be further supported in the future.
---
--- @author Bernd Brassel, Michael Hanus
--- @version January 2006

module KeyDB(existsDBKey,allDBKeys,getDBInfo,getDBInfos,
             deleteDBEntry,updateDBEntry,newDBEntry,cleanDB,
             index,sortByIndex,groupByIndex) where

import Dynamic
import Integer(maxlist)
import Sort
import List

--- Exists an entry with a given key in the database?
--- @param db - the database (a dynamic predicate)
--- @param key - a key (an integer)
existsDBKey :: (Int -> _ -> Dynamic) -> Int -> IO Bool
existsDBKey db key = seq db $ seq key $ do
  entries <- getDynamicSolution (\info -> db key info)
  return (entries /= Nothing)

--- Returns all keys of entries in the database.
allDBKeys :: (Int -> _ -> Dynamic) -> IO [Int]
allDBKeys db = seq db $ do
  getDynamicSolutions (\key -> db key unknown)

--- Gets the information about an entry in the database.
--- @param db - the database (a dynamic predicate)
--- @param key - the key of the entry (an integer)
getDBInfo :: (Int -> a -> Dynamic) -> Int -> IO a
getDBInfo db key = seq db $ seq key $ do
  entries <- getDynamicSolutions (\info -> db key info)
  if null entries
   then error ("getDBInfo: no entry for key '"++show key++"'")
   else return (head entries)


--- compute the position of an entry in a list
--- fail, if given entry is not an element.
--- @param x - the entry searched for
--- @param xs - the list to search in

index :: a -> [a] -> Int
index x xs = idx 0 xs
  where
    idx n (y:ys) = if x==y then n else idx (n+1) ys


--- Sorts a given list by associated index .
sortByIndex :: [(Int,b)] -> [b]
sortByIndex = map snd . mergeSort (\x y -> fst x < fst y) 

--- Sorts a given list by associated index and group for identical index.
--- Empty lists are added for missing indexes
groupByIndex :: [(Int,b)] -> [[b]]
groupByIndex = addEmptyIdxs 0 . groupBy   (\x y -> fst x == fst y) 
                              . mergeSort (\x y -> fst x <  fst y)
  where
    addEmptyIdxs _ [] = []
    addEmptyIdxs n (((m,x):xs):ys) = 
       if n==m then (x:map snd xs) : addEmptyIdxs (n+1) ys
               else []:addEmptyIdxs (n+1) (((m,x):xs):ys)

--- Gets the information about a list of entries in the database.
--- @param db - the database (a dynamic predicate)
--- @param keys - the list of keys of the entry (integers)
getDBInfos :: (Int -> a -> Dynamic) -> [Int] -> IO [a]
getDBInfos db keys = seq db $ seq (normalForm keys) $ do
  entries <- getDynamicSolutions (\ (i,info) -> let key free in
                db key info |&> (i=:=index key keys))
  return (sortByIndex entries)

--- Deletes an entry with a given key in the database.
--- @param db - the database (a dynamic predicate)
--- @param key - the key of the entry (an integer)
deleteDBEntry :: (Int -> _ -> Dynamic) -> Int -> IO ()
deleteDBEntry db key = seq db $ seq key $ do
  entries <- getDynamicSolutions (\infos -> db key infos)
  mapIO_ (\infos -> retract (db key infos)) entries

--- Overwrites an existing entry in the database.
--- @param db - the database (a dynamic predicate)
--- @param key - the key of the entry (an integer)
--- @param info - the information to be stored in the updated entry
updateDBEntry :: (Int -> a -> Dynamic) -> Int -> a -> IO ()
updateDBEntry db key info = do
  deleteDBEntry db key
  assert (db key info)

--- Return a new database key.
newDBKey :: (Int -> _ -> Dynamic) -> IO Int
newDBKey db = do
  ids <- getDynamicSolutions (\i -> db i unknown)
  return (if null ids then 1 else maxlist ids + 1)

--- Stores a new entry in the database and return the key of the new entry.
--- @param db - the database (a dynamic predicate)
--- @param info - the information to be stored in the new entry
newDBEntry :: (Int -> a -> Dynamic) -> a -> IO Int
newDBEntry db info = do
  i <- newDBKey db
  assert (db i info)
  return i

--- Deletes all entries in the database.
cleanDB :: (Int -> _ -> Dynamic) -> IO ()
cleanDB db = do
  ids <- getDynamicSolutions (\i -> db i unknown)
  mapIO_ (\i -> deleteDBEntry db i) ids
