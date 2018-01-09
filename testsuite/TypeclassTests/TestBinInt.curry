-- Tests for user-defined instance of Num: integers in binary representation.
-- This representation supports narrowing in integers.
-- The implementation of the operations is mainly taken from the KiCS2 compiler.

import Test.Prop

----------------------------------------------------------------------------
-- A binary representation of positive natural numbers:

data Nat = IHi | O Nat | I Nat
  deriving Eq

instance Show Nat where
  showsPrec _ x1 = showTerm 1 0 x1
    where
    showTerm a c IHi   = shows    (a + c)
    showTerm a c (O n) = showTerm (2 * a) c       n
    showTerm a c (I n) = showTerm (2 * a) (c + a) n

--- comparison, O(min (m,n))
cmpNat :: Nat -> Nat -> Ordering
cmpNat IHi   IHi   = EQ
cmpNat IHi   (O _) = LT
cmpNat IHi   (I _) = LT
cmpNat (O _) IHi   = GT
cmpNat (O x) (O y) = cmpNat x y
cmpNat (O x) (I y) = case cmpNat x y of
  EQ    -> LT
  cmpxy -> cmpxy
cmpNat (I _) IHi   = GT
cmpNat (I x) (O y) = case cmpNat x y of
  EQ    -> GT
  cmpxy -> cmpxy
cmpNat (I x) (I y) = cmpNat x y

--- successor, O(n)
succ :: Nat -> Nat
succ IHi    = O IHi        -- 1       + 1 = 2
succ (O bs) = I bs         -- 2*n     + 1 = 2*n + 1
succ (I bs) = O (succ bs)  -- 2*n + 1 + 1 = 2*(n+1)

--- predecessor, O(n)
pred :: Nat -> Nat
pred IHi         = failed     -- 1 has no predecessor
pred (O IHi)     = IHi        -- 2           - 1 = 1
pred (O x@(O _)) = I (pred x) -- 2*2*n       - 1 = 2*(2*n-1) + 1
pred (O (I x))   = I (O x)    -- 2*(2*n + 1) - 1 = 2*2*n + 1
pred (I x)       = O x        -- 2*n + 1      -1 = 2*n

--- addition, O(max (m, n))
(+^) :: Nat -> Nat -> Nat
IHi +^ y   = succ y           -- 1  +  n   = n + 1
O x +^ IHi = I x              -- 2*n + 1   = 2*n + 1
O x +^ O y = O (x +^ y)       -- 2*m + 2*n = 2*(m+n)
O x +^ I y = I (x +^ y)
I x +^ IHi = O (succ x)
I x +^ O y = I (x +^ y)
I x +^ I y = O (succ x +^ y)

--- subtraction
(-^) :: Nat -> Nat -> BinInt
IHi     -^ y     = inc (Neg y)           -- 1-n = 1+(-n)
x@(O _) -^ IHi   = Pos (pred x)          --
(O x)   -^ (O y) = mult2 (x -^ y)
(O x)   -^ (I y) = dec (mult2 (x -^ y))
(I x)   -^ IHi   = Pos (O x)
(I x)   -^ (O y) = inc (mult2 (x -^ y))  -- 2*n+1 - 2*m = 1+2*(n-m)
(I x)   -^ (I y) = mult2 (x -^ y)        -- 2*n+1 - (2*m+1) = 2*(n-m)

--- multiplication by 2
mult2 :: BinInt -> BinInt
mult2 (Pos n) = Pos (O n)
mult2 Zero    = Zero
mult2 (Neg n) = Neg (O n)

--- multiplication, O(m*n)
(*^) :: Nat -> Nat -> Nat
IHi   *^ y = y
(O x) *^ y = O (x *^ y)
(I x) *^ y = y +^ (O (x *^ y))
-- (I x) *^ IHi = I x
-- (I x) *^ (O y) = (O y) +^ (O (x *^ (O y))) = O (y +^ (x *^ (O y)))
-- (I x) *^ (I y) = (I y) +^ (O (x *^ (I y))) = I (y +^ (x *^ (I y)))


----------------------------------------------------------------------------
data BinInt = Neg Nat | Zero | Pos Nat
  deriving Eq

instance Show BinInt where
  showsPrec d (Neg x) = showString "(-" . showsPrec d x . showChar ')'
  showsPrec _ Zero    = showChar '0'
  showsPrec d (Pos x) = showsPrec d x

--- comparison on BinInt, O(min (m, n))
cmpInteger :: BinInt -> BinInt -> Ordering
cmpInteger Zero    Zero    = EQ
cmpInteger Zero    (Pos _) = LT
cmpInteger Zero    (Neg _) = GT
cmpInteger (Pos _) Zero    = GT
cmpInteger (Pos x) (Pos y) = cmpNat x y
cmpInteger (Pos _) (Neg _) = GT
cmpInteger (Neg _) Zero    = LT
cmpInteger (Neg _) (Pos _) = LT
cmpInteger (Neg x) (Neg y) = cmpNat y x

--- increment
inc :: BinInt -> BinInt
inc Zero        = Pos IHi
inc (Pos n)     = Pos (succ n)
inc (Neg IHi)   = Zero
inc (Neg (O n)) = Neg (pred (O n))
inc (Neg (I n)) = Neg (O n)

--- decrement
dec :: BinInt -> BinInt
dec Zero        = Neg IHi
dec (Pos IHi)   = Zero
dec (Pos (O n)) = Pos (pred (O n))
dec (Pos (I n)) = Pos (O n)
dec (Neg n)     = Neg (succ n)

--- Adds two BinInts.
(+#)   :: BinInt -> BinInt -> BinInt
Zero      +# x     = x
x@(Pos _) +# Zero  = x
Pos x     +# Pos y = Pos (x +^ y)
Pos x     +# Neg y = x -^ y
x@(Neg _) +# Zero  = x
Neg x     +# Pos y = y -^ x
Neg x     +# Neg y = Neg (x +^ y)

--- Subtracts two BinInts.
(-#)   :: BinInt -> BinInt -> BinInt
x -# Zero  = x
x -# Pos y = x +# Neg y
x -# Neg y = x +# Pos y

--- Multiplies two BinInts.
(*#)   :: BinInt -> BinInt -> BinInt
Zero  *# _     = Zero
Pos _ *# Zero  = Zero
Pos x *# Pos y = Pos (x *^ y)
Pos x *# Neg y = Neg (x *^ y)
Neg _ *# Zero  = Zero
Neg x *# Pos y = Neg (x *^ y)
Neg x *# Neg y = Pos (x *^ y)

toNat :: Int -> Nat
toNat n | n==1 = IHi
        | mod n 2 == 0 = O (toNat (div n 2))
        | otherwise    = I (toNat (div (n-1) 2))
        
instance Num BinInt where
  m + n = m +# n
  m - n = m -# n
  m * n = m *# n

  abs n = case n of Neg m -> Pos m
                    _     -> n

  signum (Pos _) = Pos IHi
  signum Zero    = Zero
  signum (Neg _) = Neg IHi

  fromInt n | n==0      = Zero
            | n>0       = Pos (toNat n)
            | otherwise = Neg (toNat (0 - n))


testZeroPlus3 = Zero + 3 -=- 3 + Zero

testZeroMult3 = Zero * 3 -=- 0

testZeroMultAny x = Zero * x -=- 0

testPlus :: Int -> Int -> Prop
testPlus x y  = (fromInt x) +# (fromInt y) -=- fromInt (x + y)

testMinus :: Int -> Int -> Prop
testMinus x y = (fromInt x) -# (fromInt y) -=- fromInt (x - y)

testMult :: Int -> Int -> Prop
testMult x y  = (fromInt x) *# (fromInt y) -=- fromInt (x * y)

-- test narrowing on binary numbers:
testNarrMultTwo = solutionOf (\ (x,y) -> x * y == (2::BinInt))
                  <~> ((1,2) ? (2,1) ? (-2,-1) ? (-1,-2))
