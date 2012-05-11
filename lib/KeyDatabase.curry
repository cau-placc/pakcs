---------------------------------------------------------------------------
--- This module provides a general interface for databases
--- (persistent predicates) where each entry consists of a key and an info
--- part. The key is an integer and the info is arbitrary.
--- All functions are parameterized with a dynamic predicate that
--- takes an integer key as a first parameter.
---
--- @author Bernd Brassel, Michael Hanus
--- @version August 2011
---------------------------------------------------------------------------

module KeyDatabase(existsDBKey,allDBKeys,allDBInfos,allDBKeyInfos,
                   getDBInfo,getDBInfos,
                   deleteDBEntry,deleteDBEntries,
                   updateDBEntry,newDBEntry,newDBKeyEntry,
                   cleanDB,
                   index,sortByIndex,groupByIndex,
                   module Database) where

import Database
import Integer(maxlist)
import Sort
import List
import Maybe(sequenceMaybe)

--- The type of a database key is an integer.
type Key = Int

--- Each entry of a key database contains a key and an information component.
type KeyPred a = Key -> a -> Dynamic

--- Exists an entry with a given key in the database?
--- @param db - the database (a dynamic predicate)
--- @param key - a key (an integer)
existsDBKey :: KeyPred _ -> Key -> Query Bool
existsDBKey db key = seq db $ seq key $
  transformQ (/=Nothing) (queryOne (db key))

--- Query that returns all keys of entries in the database.
allDBKeys :: KeyPred _ -> Query [Key]
allDBKeys db = seq db $ queryAll (\key -> db key unknown)

--- Query that returns all infos of entries in the database.
allDBInfos :: KeyPred a -> Query [a]
allDBInfos db = seq db $ queryAll (db unknown)

--- Query that returns all key/info pairs of the database.
allDBKeyInfos :: KeyPred a -> Query [(Key,a)]
allDBKeyInfos db = seq db $ queryAll (\ (key,info) -> db key info)

--- Gets the information about an entry in the database.
--- @param db - the database (a dynamic predicate)
--- @param key - the key of the entry (an integer)
getDBInfo :: KeyPred a -> Key -> Query (Maybe a)
getDBInfo db key = seq db $ seq key $ queryOne (\info -> db key info)

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
getDBInfos :: KeyPred a -> [Key] -> Query (Maybe [a])
getDBInfos db keys = seq db $ seq (normalForm keys) $
  transformQ sortByKeys
             (queryAll (\ (key,info) -> db key info |> key `elem` keys))
 where
   sortByKeys keyinfos = sequenceMaybe (map (\k -> lookup k keyinfos) keys)

--- Deletes an entry with a given key in the database.
--- No error is raised if the given key does not exist.
--- @param db - the database (a dynamic predicate)
--- @param key - the key of the entry (an integer)
deleteDBEntry :: KeyPred _ -> Key -> Transaction ()
deleteDBEntry db key = seq db $ seq key $
  getDB (queryAll (\infos -> db key infos)) |>>= \entries ->
  mapT_ (\infos -> deleteDB (db key infos)) entries

--- Deletes all entries with the given keys in the database.
--- No error is raised if some of the given keys does not exist.
--- @param db - the database (a dynamic predicate)
--- @param keys - the list of keys of the entries to be deleted
deleteDBEntries :: KeyPred _ -> [Key] -> Transaction ()
deleteDBEntries db keys = seq db $ seq keys $ mapT_ (deleteDBEntry db) keys

--- Overwrites an existing entry in the database.
--- @param db - the database (a dynamic predicate)
--- @param key - the key of the entry (an integer)
--- @param info - the information to be stored in the updated entry
updateDBEntry :: KeyPred a -> Key -> a -> Transaction ()
updateDBEntry db key info =
  getDB (existsDBKey db key) |>>= \b ->
  if b then deleteDBEntry db key |>> addDB (db key info)
       else errorT (TError KeyNotExistsError
                      ("updateDBEntry: key " ++ show key++" does not exist"))

--- A query that returns a new database key.
newDBKey :: KeyPred _ -> Query Key
newDBKey db = 
  transformQ (\ids -> if null ids then 1 else maxlist ids + 1)
             (queryAll (\i -> db i unknown))

--- Stores a new entry in the database and return the key of the new entry.
--- @param db - the database (a dynamic predicate)
--- @param info - the information to be stored in the new entry
newDBEntry :: KeyPred a -> a -> Transaction Key
newDBEntry db info =
  getDB (newDBKey db) |>>= \i -> addDB (db i info) |>> returnT i

--- Stores a new entry in the database under a given key.
--- The transaction fails if the key already exists.
--- @param db - the database (a dynamic predicate)
--- @param key - the key of the new entry (an integer)
--- @param info - the information to be stored in the new entry
newDBKeyEntry :: KeyPred a -> Key -> a -> Transaction ()
newDBKeyEntry db key info =
  getDB (existsDBKey db key) |>>= \b ->
  if b then errorT (TError DuplicateKeyError
                      ("database already contains entry with key: "++show key))
       else addDB (db key info) |>> doneT

--- Deletes all entries in the database.
cleanDB :: KeyPred _ -> Transaction ()
cleanDB db = getDB (queryAll (\i -> db i unknown)) |>>= deleteDBEntries db
