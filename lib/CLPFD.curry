------------------------------------------------------------------------------
--- Library for finite domain constraint solving.
--- <p>
--- The general structure of a specification of an FD problem is as follows:
--- 
--- <code>domain_constraint & fd_constraint & labeling</code>
--- 
--- where:
--- 
--- <code>domain_constraint</code>
--- specifies the possible range of the FD variables (see constraint <code>domain</code>)
--- 
--- <code>fd_constraint</code>
--- specifies the constraint to be satisfied by a valid solution
--- (see constraints #+, #-, allDifferent, etc below)
--- 
--- <code>labeling</code>
--- is a labeling function to search for a concrete solution.
---
--- Note: This library is based on the corresponding library of Sicstus-Prolog
--- but does not implement the complete functionality of the
--- Sicstus-Prolog library.
--- However, using the PAKCS interface for external functions, it is relatively
--- easy to provide the complete functionality.
---
--- @author Michael Hanus
--- @version June 2012
------------------------------------------------------------------------------

module CLPFD(domain, (+#), (-#), (*#), (=#), (/=#), (<#), (<=#), (>#), (>=#),
             Constraint, (#=#), (#/=#), (#<#), (#<=#), (#>#), (#>=#),
             neg, (#/\#), (#\/#), (#=>#), (#<=>#), solve,
             sum, scalarProduct, allDifferent, all_different, count,
             indomain, labeling, LabelingOption(..)) where

-- The operator declarations are similar to the standard arithmetic
-- and relational operators.

infixl 7 *#
infixl 6 +#, -#
infix  4 =#, /=#, <#, <=#, >#, >=#
infix  4 #=#, #/=#, #<#, #<=#, #>#, #>=#
infixr 3 #/\#
infixr 2 #\/#
infixr 1 #=>#, #<=>#


--- Constraint to specify the domain of all finite domain variables.
--- @param xs - list of finite domain variables
--- @param min - minimum value for all variables in xs
--- @param max - maximum value for all variables in xs

domain :: [Int] -> Int -> Int -> Success
domain vs l u = ((prim_domain $!! (ensureSpine vs)) $# l) $# u

prim_domain :: [Int] -> Int -> Int -> Success
prim_domain external

--- Addition of FD variables.
(+#)   :: Int -> Int -> Int
x +# y = (prim_FD_plus $! y) $! x

prim_FD_plus :: Int -> Int -> Int
prim_FD_plus external

--- Subtraction of FD variables.
(-#)   :: Int -> Int -> Int
x -# y = (prim_FD_minus $! y) $! x

prim_FD_minus :: Int -> Int -> Int
prim_FD_minus external

--- Multiplication of FD variables.
(*#)   :: Int -> Int -> Int
x *# y = (prim_FD_times $! y) $! x

prim_FD_times :: Int -> Int -> Int
prim_FD_times external

--- Equality of FD variables.
(=#)   :: Int -> Int -> Success
x =# y = (prim_FD_equal $! y) $! x

prim_FD_equal :: Int -> Int -> Success
prim_FD_equal external

--- Disequality of FD variables.
(/=#)  :: Int -> Int -> Success
x /=# y = (prim_FD_notequal $! y) $! x

prim_FD_notequal :: Int -> Int -> Success
prim_FD_notequal external

--- "Less than" constraint on FD variables.
(<#)   :: Int -> Int -> Success
x <# y = (prim_FD_le $! y) $! x

prim_FD_le :: Int -> Int -> Success
prim_FD_le external

--- "Less than or equal" constraint on FD variables.
(<=#)  :: Int -> Int -> Success
x <=# y = (prim_FD_leq $! y) $! x

prim_FD_leq :: Int -> Int -> Success
prim_FD_leq external

--- "Greater than" constraint on FD variables.
(>#)   :: Int -> Int -> Success
x ># y = (prim_FD_ge $! y) $! x

prim_FD_ge :: Int -> Int -> Success
prim_FD_ge external

--- "Greater than or equal" constraint on FD variables.
(>=#)  :: Int -> Int -> Success
x >=# y = (prim_FD_geq $! y) $! x

prim_FD_geq :: Int -> Int -> Success
prim_FD_geq external

---------------------------------------------------------------------------
-- Reifyable constraints.

--- A datatype to represent reifyable constraints.
data Constraint = FDEqual Int Int
                | FDNotEqual Int Int
                | FDGreater Int Int
                | FDGreaterOrEqual Int Int
                | FDLess Int Int
                | FDLessOrEqual Int Int
                | FDNeg   Constraint
                | FDAnd   Constraint Constraint
                | FDOr    Constraint Constraint
                | FDImply Constraint Constraint
                | FDEquiv Constraint Constraint

--- Reifyable equality constraint on FD variables.
(#=#)  :: Int -> Int -> Constraint
x #=# y = FDEqual x y

--- Reifyable inequality constraint on FD variables.
(#/=#)  :: Int -> Int -> Constraint
x #/=# y = FDNotEqual x y

--- Reifyable "less than" constraint on FD variables.
(#<#)  :: Int -> Int -> Constraint
x #<# y = FDLess x y

--- Reifyable "less than or equal" constraint on FD variables.
(#<=#)  :: Int -> Int -> Constraint
x #<=# y = FDLessOrEqual x y

--- Reifyable "greater than" constraint on FD variables.
(#>#)  :: Int -> Int -> Constraint
x #># y = FDGreater x y

--- Reifyable "greater than or equal" constraint on FD variables.
(#>=#)  :: Int -> Int -> Constraint
x #>=# y = FDGreaterOrEqual x y

--- The resulting constraint is satisfied if both argument constraints
--- are satisfied.
neg :: Constraint -> Constraint
neg x = FDNeg x

--- The resulting constraint is satisfied if both argument constraints
--- are satisfied.
(#/\#)  :: Constraint -> Constraint -> Constraint
x #/\# y = FDAnd x y

--- The resulting constraint is satisfied if both argument constraints
--- are satisfied.
(#\/#)  :: Constraint -> Constraint -> Constraint
x #\/# y = FDOr x y

--- The resulting constraint is satisfied if the first argument constraint
--- do not hold or both argument constraints are satisfied.
(#=>#)  :: Constraint -> Constraint -> Constraint
x #=># y = FDImply x y

--- The resulting constraint is satisfied if both argument constraint
--- are either satisfied and do not hold.
(#<=>#)  :: Constraint -> Constraint -> Constraint
x #<=># y = FDEquiv x y

--- Solves a reified constraint.
solve :: Constraint -> Success
solve c = prim_solve_reify $!! c

prim_solve_reify :: Constraint -> Success
prim_solve_reify external

---------------------------------------------------------------------------
-- Complex constraints.

--- Relates the sum of FD variables with some integer of FD variable.
sum :: [Int] -> (Int -> Int -> Success) -> Int -> Success
sum vs rel v = seq (normalForm (ensureSpine vs))
                   (seq (ensureNotFree rel) (seq v (prim_sum vs rel v)))

prim_sum :: [Int] -> (Int -> Int -> Success) -> Int -> Success
prim_sum external

--- (scalarProduct cs vs relop v) is satisfied if ((cs*vs) relop v) is satisfied.
--- The first argument must be a list of integers. The other arguments are as
--- in <code>sum</code>.
scalarProduct :: [Int] -> [Int] -> (Int -> Int -> Success) -> Int -> Success
scalarProduct cs vs rel v =
  seq (groundNormalForm cs)
      (seq (normalForm (ensureSpine vs))
           (seq (ensureNotFree rel) (seq v (prim_scalarProduct cs vs rel v))))

prim_scalarProduct :: [Int] -> [Int] -> (Int -> Int -> Success) -> Int -> Success
prim_scalarProduct external


--- (count v vs relop c) is satisfied if (n relop c), where n is the number of
--- elements in the list of FD variables vs that are equal to v, is satisfied.
--- The first argument must be an integer. The other arguments are as
--- in <code>sum</code>.
count :: Int -> [Int] -> (Int -> Int -> Success) -> Int -> Success
count v vs rel c =
  seq (ensureNotFree v)
      (seq (normalForm (ensureSpine vs))
           (seq (ensureNotFree rel) (seq c (prim_count v vs rel c))))

prim_count :: Int -> [Int] -> (Int -> Int -> Success) -> Int -> Success
prim_count external


--- "All different" constraint on FD variables.
--- @param xs - list of FD variables
--- @return satisfied if the FD variables in the argument list xs
---         have pairwise different values.

allDifferent :: [Int] -> Success
allDifferent vs = seq (normalForm (ensureSpine vs)) (prim_allDifferent vs)

--- For backward compatibility. Use <code>allDifferent</code>.
all_different :: [Int] -> Success
all_different = allDifferent

prim_allDifferent :: [Int] -> Success
prim_allDifferent external

--- Instantiate a single FD variable to its values in the specified domain.
indomain :: Int -> Success
indomain x = seq x (prim_indomain x)

prim_indomain :: Int -> Success
prim_indomain external


---------------------------------------------------------------------------
-- Labeling.

--- Instantiate FD variables to their values in the specified domain.
--- @param options - list of option to control the instantiation of FD variables
--- @param xs - list of FD variables that are non-deterministically
---             instantiated to their possible values.

labeling :: [LabelingOption] -> [Int] -> Success
labeling options vs = seq (normalForm (map ensureNotFree (ensureSpine options)))
                          (seq (normalForm (ensureSpine vs))
                               (prim_labeling options vs))

prim_labeling :: [LabelingOption] -> [Int] -> Success
prim_labeling external

--- This datatype contains all options to control the instantiated of FD variables
--- with the enumeration constraint <code>labeling</code>.
--- @cons LeftMost - The leftmost variable is selected for instantiation (default)
--- @cons FirstFail - The leftmost variable with the smallest domain is selected
---                   (also known as first-fail principle)
--- @cons FirstFailConstrained - The leftmost variable with the smallest domain
---                              and the most constraints on it is selected.
--- @cons Min - The leftmost variable with the smalled lower bound is selected.
--- @cons Max - The leftmost variable with the greatest upper bound is selected.
--- @cons Step - Make a binary choice between <code>x=#b</code> and
---              <code>x/=#b</code> for the selected variable
---              <code>x</code> where <code>b</code> is the lower or
---              upper bound of <code>x</code> (default).
--- @cons Enum - Make a multiple choice for the selected variable for all the values
---              in its domain.
--- @cons Bisect - Make a binary choice between <code>x&lt;=#m</code> and
---                <code>x&gt;#m</code> for the selected variable
---                <code>x</code> where <code>m</code> is the midpoint
---                of the domain <code>x</code>
---                (also known as domain splitting).
--- @cons Up - The domain is explored for instantiation in ascending order (default).
--- @cons Down - The domain is explored for instantiation in descending order.
--- @cons All - Enumerate all solutions by backtracking (default).
--- @cons Minimize v - Find a solution that minimizes the domain variable v
---                    (using a branch-and-bound algorithm).
--- @cons Maximize v - Find a solution that maximizes the domain variable v
---                    (using a branch-and-bound algorithm).
--- @cons Assumptions x - The variable x is unified with the number of choices
---                       made by the selected enumeration strategy when a solution
---                       is found.

data LabelingOption = LeftMost
                    | FirstFail
                    | FirstFailConstrained
                    | Min
                    | Max
                    | Step
                    | Enum
                    | Bisect
                    | Up
                    | Down
                    | All
                    | Minimize Int
                    | Maximize Int
                    | Assumptions Int


-- end of library CLPFD
