------------------------------------------------------------------------------
--- Library for constraint programming with arithmetic constraints over reals.
------------------------------------------------------------------------------

module CLPR((+.),(-.),(*.),(/.),(<.),(>.),(<=.),(>=.),i2f,
            minimumFor,minimize,maximumFor,maximize) where

-- The operator declarations are similar to the standard arithmetic operators.

infixl 7 *., /.
infixl 6 +., -.
infix  4 <., >., <=., >=.


--- Addition on floats in arithmetic constraints.

(+.)   :: Float -> Float -> Float
x +. y = (prim_CLPR_plus $! y) $! x

prim_CLPR_plus :: Float -> Float -> Float
prim_CLPR_plus external

--- Subtraction on floats in arithmetic constraints.

(-.)   :: Float -> Float -> Float
x -. y = (prim_CLPR_minus $! y) $! x

prim_CLPR_minus :: Float -> Float -> Float
prim_CLPR_minus external

--- Multiplication on floats in arithmetic constraints.

(*.)   :: Float -> Float -> Float
x *. y = (prim_CLPR_times $! y) $! x

prim_CLPR_times :: Float -> Float -> Float
prim_CLPR_times external

--- Division on floats in arithmetic constraints.

(/.)   :: Float -> Float -> Float
x /. y = (prim_CLPR_div $! y) $! x

prim_CLPR_div :: Float -> Float -> Float
prim_CLPR_div external

--- "Less than" constraint on floats.

(<.)   :: Float -> Float -> Success
x <. y = (prim_CLPR_le $! y) $! x

prim_CLPR_le :: Float -> Float -> Success
prim_CLPR_le external

--- "Greater than" constraint on floats.

(>.)   :: Float -> Float -> Success
x >. y = (prim_CLPR_ge $! y) $! x

prim_CLPR_ge :: Float -> Float -> Success
prim_CLPR_ge external

--- "Less than or equal" constraint on floats.

(<=.)  :: Float -> Float -> Success
x <=. y = (prim_CLPR_leq $! y) $! x

prim_CLPR_leq :: Float -> Float -> Success
prim_CLPR_leq external

--- "Greater than or equal" constraint on floats.

(>=.)  :: Float -> Float -> Success
x >=. y = (prim_CLPR_geq $! y) $! x

prim_CLPR_geq :: Float -> Float -> Success
prim_CLPR_geq external

--- Conversion function from integers to floats.
--- Rigid in the first argument, i.e., suspends until the first argument
--- is ground.

i2f    :: Int -> Float
i2f x = prim_CLPR_i2f $# x

prim_CLPR_i2f :: Int -> Float
prim_CLPR_i2f external


--- Computes the minimum with respect to a given constraint.
--- (minimumFor g f) evaluates to x if (g x) is satisfied and
--- (f x) is minimal. The evaluation fails if such a minimal value
--- does not exist. The evaluation suspends if it contains
--- unbound non-local variables.

minimumFor :: (a -> Success) -> (a -> Float) -> a
minimumFor external

--- Minimization constraint.
--- (minimize g f x) is satisfied if (g x) is satisfied and
--- (f x) is minimal. The evaluation suspends if it contains
--- unbound non-local variables.

minimize :: (a -> Success) -> (a -> Float) -> a -> Success
minimize g f x = minimumFor g f =:= x

--- Computes the maximum with respect to a given constraint.
--- (maximumFor g f) evaluates to x if (g x) is satisfied and
--- (f x) is maximal. The evaluation fails if such a maximal value
--- does not exist. The evaluation suspends if it contains
--- unbound non-local variables.

maximumFor :: (a -> Success) -> (a -> Float) -> a
maximumFor external

--- Maximization constraint.
--- (maximize g f x) is satisfied if (g x) is satisfied and
--- (f x) is maximal. The evaluation suspends if it contains
--- unbound non-local variables.

maximize :: (a -> Success) -> (a -> Float) -> a -> Success
maximize g f x = maximumFor g f =:= x


-- end of CLPR
