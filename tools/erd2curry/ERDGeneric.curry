------------------------------------------------------------------------------
--- Generic operations and integrity tests
--- to support the database code generated from ERDs
------------------------------------------------------------------------------

module ERDGeneric where

import Database
import List
import ReadShowTerm
import Read
import Char(isDigit)

------------------------------------------------------------------------------
-- Handling of database keys

--- The general type of database keys.
type Key = Int

--- Shows a database key for an entity name as a string.
--- Useful if a textual representation of a database key is necessary,
--- e.g., as URL parameters in web pages. This textual representation
--- should not be used to store database keys in attributes!
showDatabaseKey :: String -> (enkey -> Key) -> enkey -> String
showDatabaseKey en fromenkey enkey = en ++ show (fromenkey enkey)

--- Transforms a string into a key for an entity name.
--- Nothing is returned if the string does not represent a reasonable key.
readDatabaseKey :: String -> (Key -> enkey) -> String -> Maybe enkey
readDatabaseKey en toenkey s =
  let (ens,ks) = splitAt (length en) s
   in if ens==en && all isDigit ks then Just (toenkey (readNat ks))
                                   else Nothing


------------------------------------------------------------------------------
-- Generic operations to modify the database

--- Insert a new entity and assign a new key for it.
newEntry :: (en -> Key) -> (en -> Key -> en) -> (en -> Dynamic) -> en
            -> Transaction en
newEntry keyf keyset pred entry =
  newDBKey keyf pred |>>= \k ->
  let entrywithkey = keyset entry k in
  addDB (pred entrywithkey) |>> returnT entrywithkey

--- Gets a new key for a dynamic predicate w.r.t. a given key function.
newDBKey :: (en -> Key) -> (en -> Dynamic) -> Transaction Key
newDBKey keyf pred =
  getDB (queryAll pred) |>>= \entries ->
  returnT (if null entries then 1 else foldr1 max (map keyf entries) + 1)

-- Insert new relationship represented as an entity.
newEntryR :: a -> b -> (a -> b -> Dynamic) -> Transaction ()
newEntryR key1 key2 pred = addDB $ pred key1 key2

getEntry :: k -> (k -> en -> Dynamic) -> Transaction en
getEntry key pred = seq pred $ seq key $
  getDB (queryOne (\info -> pred key info)) |>>=
  maybe (errorT (TError KeyNotExistsError
                        ("database contains no entry for key: "++show key 
                                 ++" table: "++show pred)))
        returnT

getEntryR :: k1 -> k2 -> (a -> k1) -> (a -> k2) -> (a -> Dynamic)
             -> Transaction a
getEntryR key1 key2 sel1 sel2 pred =
  getDB (queryOne
          (\info -> pred info |> (sel1 info == key1 && sel2 info == key2))) |>>=
  maybe (errorT (TError NoRelationshipError 
                        ("no relationship found for keys: "
                         ++show key1++" "++show key2)))
        returnT

updateEntry :: (a -> b) -> (b -> a -> Dynamic) -> a -> Transaction ()
updateEntry keyf pred entry =
  deleteEntry keyf pred entry |>>
  addDB (pred (keyf entry) entry)

deleteEntry :: (a -> b) -> (b -> a -> Dynamic) -> a -> Transaction ()
deleteEntry keyf pred entry = seq pred $ seq (keyf entry) $
  getDB (queryAll (\info -> pred (keyf entry) info)) |>>=
  mapT_ (\info -> deleteDB (pred (keyf entry) info))

-- Delete a relationship represented as an entity.
-- If the relationship does not exist, a NoRelationshipError is raised.
deleteEntryR :: a -> b -> (a -> b -> Dynamic) -> Transaction ()
deleteEntryR key1 key2 pred =
  getDB (queryOne (\ () -> pred key1 key2)) |>>=
  maybe (errorT (TError NoRelationshipError 
                        ("relationship for deletion not found for keys: "
                         ++show key1++" "++show key2)))
        (const (deleteDB $ pred key1 key2))


------------------------------------------------------------------------------
-- Generic integrity tests for keys.

-- key :: ENKey
-- keyf = enKey
existsDBKey :: (en -> k) -> (en -> Dynamic) -> k -> Transaction ()
existsDBKey keyf pred key =
  getDB (queryOne (\info -> pred info |> key == keyf info)) |>>=
  maybe (errorT (TError KeyNotExistsError
                        ("database contains no entry for key: "++show key 
                         ++" table: "++show pred))  )
        (const doneT)

-- If a given key occurs in a (foreign key) attribute of an entity,
-- raise a error.
requiredForeignDBKey :: (a -> k) -> (a -> Dynamic) -> k -> Transaction ()
requiredForeignDBKey keyf pred key =
  getDB (queryOne (\info -> pred info |> key == keyf info)) |>>=
  maybe doneT
        (const (errorT (TError KeyRequiredError
                            ("key: "++show key ++
                             " required in table: " ++ show pred)) ))

duplicateKeyTest :: (k -> a -> Dynamic) -> Transaction ()
duplicateKeyTest pred =
  getDB (queryAll (\k -> pred k unknown)) |>>= \keys ->
  if length (nub keys) == length keys
     then doneT
     else errorT (TError DuplicateKeyError
                             ("database contains duplicate key for table: " 
                                ++show pred)) 
     
duplicatePTest :: [a] -> Transaction ()
duplicatePTest xs =
  if length (nub xs) == length xs
  then doneT
  else errorT (TError DuplicateKeyError "duplicate parameters in new-function")


-------------------------------------------------------------------------
-- Uniqueness tests.
 
-- Test whether an attribute value does not yet exist
unique :: (en -> a) -> (en -> Dynamic) -> a -> Transaction ()
unique selector pred attrval =
  getDB (queryOne (\info -> pred info |> attrval == selector info)) |>>=
  maybe doneT
        (const (errorT (TError UniqueError
                         ("entry for unique attribute "
                          ++show pred++"."++show attrval++" already exists"))))

uniqueUpdate :: (b -> k) -> (b -> a) -> (k -> b -> Dynamic) -> b
             -> Transaction ()
uniqueUpdate keyf selector pred obj =
  getDB (queryAll (\info -> 
          pred unknown info |> selector obj == selector info)) |>>= \entries ->
  getEntry (keyf obj) pred |>>= \old ->
  if (null entries || (length entries == 1 && selector old == selector obj))
     then doneT
     else errorT (TError UniqueError
                    ("entry for unique attribute "
                     ++show pred++"."++show (selector obj)++" already exists"))

uniqueC :: (b -> a) -> (k -> b -> Dynamic) -> b -> Transaction ()
uniqueC selector pred obj =
  getDB (queryAll (\info -> 
          pred unknown info |> selector obj == selector info)) |>>= \entries ->
  if length entries <= 1
     then doneT
     else errorT (TError UniqueError
                     ("unique attribute "
                      ++show pred++"."++show (selector obj)++" is not unique"))

-- Uniqueness of a combination of two attributes.
-- Check whether this combination already exists.
-- If it exists, a transaction error is generated, otherwise everything is ok.
unique2 :: (c -> a) -> (c -> b) -> (c -> Dynamic) -> a -> b -> Transaction ()
unique2 sel1 sel2 pred k1 k2 =
  getDB (queryOne
            (\info -> pred info |> (k1 == sel1 info && k2 == sel2 info))) |>>= 
  maybe doneT
        (const (errorT (TError UniqueError "relationship already exists")))

unique2C ::  (c -> a) -> (c -> b) -> (c -> Dynamic) -> a -> b -> Transaction ()
unique2C sel1 sel2 pred k1 k2 =
  getDB (queryAll (\info -> 
           pred info |> (k1 == sel1 info && k2 == sel2 info))) |>>= \entries ->
  if length entries > 1
     then errorT (TError UniqueError "relationship not unique")
     else doneT


-------------------------------------------------------------------------
-- Maximum and minimum tests.

maxPTest :: Int -> [a] -> Transaction ()
maxPTest max xs = 
  if length xs > max
  then errorT (TError MaxError "max reached in parameter list in new function")
  else doneT

maxTest :: (b -> a) -> (b -> Dynamic) -> Int -> a -> Transaction ()
maxTest selector pred max attr =
  getDB (queryAll (\info -> pred info |> attr == selector info)) |>>= \entries->
  if length entries < max
     then doneT
     else errorT (TError MaxError ("max reached for attribute " 
                                        ++show pred++"."++show attr))

maxTestUpdate :: (b -> k) -> (b -> a) -> (k -> b -> Dynamic) -> Int -> b
              -> Transaction ()
maxTestUpdate keyf selector pred max obj =
  getDB (queryAll (\info -> 
           pred unknown info |> selector obj == selector info)) |>>= \entries ->
  getEntry (keyf obj) pred |>>= \old ->
  if (length entries < max
        || (length entries == max && selector old == selector obj))
     then doneT
     else errorT (TError MaxError ("max reached for attribute "
                                        ++show pred++"."++show (selector obj)))

maxTestC :: (b -> a) -> (b -> Dynamic) -> Int -> a -> Transaction ()
maxTestC selector pred max attr =
  getDB (queryAll (\info -> pred info |> attr == selector info)) |>>= \entries->
  if length entries <= max
     then doneT
     else errorT (TError MaxError ("max exceeded for attribute " 
                                        ++show pred++"."++show attr))

minTestC :: (b -> a) -> (b -> Dynamic) -> Int -> a -> Transaction ()
minTestC selector pred min attr =
  getDB (queryAll (\info -> pred info |> attr == selector info)) |>>= \entries->
  if length entries >= min
     then doneT
     else errorT (TError MinError ("below min for attribute " 
                                        ++show pred++"."++show attr))

-- Maximum test before inserting a relationship with a given key:
maxTestInsert :: (b -> a) -> (b -> Dynamic) -> Int -> a -> Transaction ()
maxTestInsert selector pred maxrange attr =
  getDB (queryAll (\info -> pred info |> attr == selector info)) |>>= \entries->
  if length entries < maxrange
     then doneT
     else errorT (TError MaxError ("maximum reached for attribute " 
                                        ++show pred++"."++show attr))

-- Minimum test before deleting a relationship
minTestDelete :: (b -> a) -> (b -> Dynamic) -> Int -> a -> Transaction ()
minTestDelete selector pred min attr =
  getDB (queryAll (\info -> pred info |> attr == selector info)) |>>= \entries->
  if length entries > min
     then doneT
     else errorT (TError MinError ("below min for attribute " 
                                        ++show pred++"."++show attr))

-------------------------------------------------------------------------
-- Saving and restoring dynamic predicates.

saveDBTerms :: String -> String -> (a -> Dynamic) -> IO ()
saveDBTerms path ename dynpred = do
  terms <- runQ (queryAll dynpred)
  let savefile = path++"/"++ename++".terms"
  if null path
   then putStrLn (unlines (map showQTerm terms)) -- show only
   else do putStrLn $ "Saving into "++savefile
           writeQTermListFile savefile terms

restoreDBTerms :: String -> String -> (a -> Dynamic) -> IO ()
restoreDBTerms path ename dynpred = do
  let savefile = path++"/"++ename++".terms"
  putStrLn $ "Restoring from "++savefile
  terms <- readQTermListFile savefile
  runJustT (mapT_ (\t -> addDB (dynpred t)) terms)

-------------------------------------------------------------------------
-- If the second argument is a null string, return the first argument
-- (the default string), otherwise return the second argument.
defaultString :: String -> String -> String
defaultString def s = if null s then def else s

-------------------------------------------------------------------------
