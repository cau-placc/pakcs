------------------------------------------------------------------------------
--- Library to access profile data of the Prolog system
---
--- IMPORT NOTE: If this module is used, one must set the PAKCS option
--- "plprofile" (wiht ":set +plprofile") in order to make the profile
--- data available from the Prolog system!
---
--- @author Michael Hanus
--- @version February 2004
------------------------------------------------------------------------------

module PlProfileData where

-- Kind of selected profile data:
data ProfileSelection = Calls | Backtracks | ChoicePoints | ExecTime

--- Resets all profile counters.
profileReset :: IO ()
profileReset external

--- Gets profile data of all predicates.
profilePredicates :: ProfileSelection -> IO [(String,Int)]
profilePredicates ps = prim_profilePredicates $# ps

prim_profilePredicates :: ProfileSelection -> IO [(String,Int)]
prim_profilePredicates external

--- Gets profile data of all clauses of a particular predicate.
--- The result is a list of pairs (predicate number, profile count)
profileClauses :: ProfileSelection -> String -> IO [(Int,Int)]
profileClauses ps pred = (prim_profileClauses $# ps) $## pred

prim_profileClauses :: ProfileSelection -> String -> IO [(Int,Int)]
prim_profileClauses external


--- Starts graphical viewer for profile data via the Gauge profiling
--- tool of Sicstus Prolog.
profileView :: IO ()
profileView external

--- Gets the association of HNF clauses to functions symbols
--- (only for internal use).
getHnfDefinitions :: IO [(Int,String)]
getHnfDefinitions external

