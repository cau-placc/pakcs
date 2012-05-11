------------------------------------------------------------------------------
--- This module contains the definition of data types to represent
--- entity/relationship diagrams and an I/O operation to read them
--- from a term file.
---
--- @author Michael Hanus, Marion Mueller
--- @version September 2010
------------------------------------------------------------------------------

module ERD(ERD(..),ERDName,Entity(..),EName,Entity(..),
           Attribute(..),AName,Key(..),Null,Domain(..),
           Relationship(..),REnd(..),RName,Role,Cardinality(..),MaxValue(..),
           readERDTermFile) where

import Time
import ReadShowTerm(readUnqualifiedTerm)

--- Data type to represent entity/relationship diagrams.
data ERD = ERD ERDName [Entity] [Relationship]

type ERDName = String -- used as the name of the generated module


data Entity = Entity EName [Attribute]

type EName = String

data Attribute = Attribute AName Domain Key Null  

type AName = String

data Key = NoKey
         | PKey
         | Unique

type Null = Bool 

data Domain = IntDom      (Maybe Int) 
            | FloatDom    (Maybe Float)
            | CharDom     (Maybe Char)
            | StringDom   (Maybe String) 
            | BoolDom     (Maybe Bool)
            | DateDom     (Maybe CalendarTime)
            | UserDefined String (Maybe String)
            | KeyDom      String   -- later used for foreign keys


data Relationship = Relationship RName [REnd] 
 
type RName = String

data REnd = REnd EName Role Cardinality

type Role = String

--- Cardinality of a relationship w.r.t. some entity.
--- The cardinality is either a fixed number (e.g., (Exactly 1)
--- representing the cardinality (1,1))
--- or an interval (e.g., (Between 1 (Max 4)) representing the
--- cardinality (1,4), or (Between 0 Infinite) representing the
--- cardinality (0,n)).
data Cardinality = Exactly Int            
                 | Between Int MaxValue
                 | Range Int (Maybe Int) -- for backward compatibility

--- The upper bound of a cardinality which is either a finite number
--- or infinite.
data MaxValue = Max Int | Infinite


--- Read an ERD specification from a file containing a single ERD term.
readERDTermFile :: String -> IO ERD
readERDTermFile termfilename = do
  putStrLn $ "Reading ERD term from file " ++ termfilename ++ "..."
  termstring <- readFile termfilename
  return (updateERDTerm (readUnqualifiedTerm ["ERD","Prelude"] termstring))

--- Transforms an ERD term possible containing old, outdated, information.
--- In particular, translate (Range ...) into (Between ...).
updateERDTerm :: ERD -> ERD
updateERDTerm (ERD name es rs) = ERD name es (map updateRel rs)
 where
   updateRel (Relationship r ends) = Relationship r (map updateEnd ends)

   updateEnd (REnd n r c) = REnd n r (updateCard c)

   updateCard (Exactly n) = Exactly n
   updateCard (Between min (Max m)) =
     if min<=m
     then Between min (Max m)
     else error ("ERD: Illegal cardinality " ++ show (Between min (Max m)))
   updateCard (Between min Infinite) = Between min Infinite
   updateCard (Range min max) = updateCard (Between min (maybe Infinite Max max))

{-
-- Example ERD term:
(ERD "Uni"
 [Entity "Student" [Attribute "MatNum"    (IntDom Nothing) PKey False,
                    Attribute "Name"      (StringDom Nothing) NoKey False,
                    Attribute "Firstname" (StringDom Nothing) NoKey False,
                    Attribute "Email"     (UserDefined "MyModule.Email" Nothing)
                                          NoKey True],
  Entity "Lecture" [Attribute "Id"    (IntDom Nothing) PKey False,
                    Attribute "Title" (StringDom Nothing) Unique False,
                    Attribute "Hours" (IntDom (Just 4)) NoKey False],
  Entity "Lecturer" [Attribute "Id"        (IntDom Nothing) PKey False,
                     Attribute "Name"      (StringDom Nothing) NoKey False,
                     Attribute "Firstname" (StringDom Nothing) NoKey False],
  Entity "Group" [Attribute "Time" (StringDom Nothing) NoKey False]]
 [Relationship "Teaching"
               [REnd "Lecturer" "taught_by" (Exactly 1),
                REnd "Lecture"  "teaches"   (Between 0 Infinite)],
  Relationship "Participation"
               [REnd "Student" "participated_by" (Between 0 Infinite),
                REnd "Lecture" "participates"    (Between 0 Infinite)],
  Relationship "Membership"
               [REnd "Student" "consists_of" (Exactly 3),
                REnd "Group" "member_of"     (Between 0 Infinite)]])

-}
