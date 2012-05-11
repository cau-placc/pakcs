------------------------------------------------------------------------------
--- A collection of operations on floating point numbers.
------------------------------------------------------------------------------

module Float((+.),(-.),(*.),(/.),i2f,truncate,round,
             sqrt,log,exp,sin,cos,tan,atan) where

-- The operator declarations are similar to the standard arithmetic operators.

infixl 7 *., /.
infixl 6 +., -.


--- Addition on floats.
(+.)   :: Float -> Float -> Float
x +. y = (prim_Float_plus $# y) $# x

prim_Float_plus :: Float -> Float -> Float
prim_Float_plus external

--- Subtraction on floats.
(-.)   :: Float -> Float -> Float
x -. y = (prim_Float_minus $# y) $# x

prim_Float_minus :: Float -> Float -> Float
prim_Float_minus external

--- Multiplication on floats.
(*.)   :: Float -> Float -> Float
x *. y = (prim_Float_times $# y) $# x

prim_Float_times :: Float -> Float -> Float
prim_Float_times external

--- Division on floats.
(/.)   :: Float -> Float -> Float
x /. y = (prim_Float_div $# y) $# x

prim_Float_div :: Float -> Float -> Float
prim_Float_div external

--- Conversion function from integers to floats.
i2f    :: Int -> Float
i2f x = prim_i2f $# x

prim_i2f :: Int -> Float
prim_i2f external

--- Conversion function from floats to integers.
--- The result is the closest integer between the argument and 0.
truncate :: Float -> Int
truncate x = prim_truncate $# x

prim_truncate :: Float -> Int
prim_truncate external

--- Conversion function from floats to integers.
--- The result is the nearest integer to the argument.
--- If the argument is equidistant between two integers,
--- it is rounded to the closest even integer value.

round :: Float -> Int
round x = prim_round $# x

prim_round :: Float -> Int
prim_round external

--- Square root.

sqrt :: Float -> Float
sqrt x = prim_sqrt $# x

prim_sqrt :: Float -> Float
prim_sqrt external

--- Natural logarithm.

log :: Float -> Float
log x = prim_log $# x

prim_log :: Float -> Float
prim_log external

--- Natural exponent.
exp :: Float -> Float
exp x = prim_exp $# x

prim_exp :: Float -> Float
prim_exp external

--- Sine.
sin :: Float -> Float
sin x = prim_sin $# x

prim_sin :: Float -> Float
prim_sin external

--- Cosine.
cos :: Float -> Float
cos x = prim_cos $# x

prim_cos :: Float -> Float
prim_cos external

--- Tangent.
tan :: Float -> Float
tan x = prim_tan $# x

prim_tan :: Float -> Float
prim_tan external

--- Arc tangent.
atan :: Float -> Float
atan x = prim_atan $# x

prim_atan :: Float -> Float
prim_atan external
