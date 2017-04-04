-- Agda program using the Iowa Agda library

open import bool

module PROOF-evendoublecoin
  (Choice : Set)
  (choose : Choice → 𝔹)
  (lchoice : Choice → Choice)
  (rchoice : Choice → Choice)
  where

open import eq
open import nat
open import list
open import maybe

---------------------------------------------------------------------------
-- Translated Curry operations:

add : ℕ → ℕ → ℕ
add zero x = x
add (suc y) z = suc (add y z)

coin : Choice → ℕ → ℕ
coin c1 x = if choose c1 then x else suc x

double : ℕ → ℕ
double x = add x x

even : ℕ → 𝔹
even zero = tt
even (suc zero) = ff
even (suc (suc x)) = even x

---------------------------------------------------------------------------

add-suc : ∀ (x y : ℕ) → add x (suc y) ≡ suc (add x y)
add-suc zero y = refl
add-suc (suc x) y rewrite add-suc x y = refl

-- auxiliary property for x+x instead of double:
even-add-x-x : ∀ (x : ℕ) → even (add x x) ≡ tt
even-add-x-x zero = refl
even-add-x-x (suc x) rewrite add-suc x x | even-add-x-x x = refl

evendoublecoin : (c1 : Choice) → (x : ℕ) → (even (double (coin c1 x))) ≡ tt
evendoublecoin c1 x rewrite even-add-x-x (coin c1 x) = refl

---------------------------------------------------------------------------
