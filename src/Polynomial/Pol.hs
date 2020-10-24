-- |
--    Module      :  $Header$
--    Description :  Define simple type rules and type-checking.
--    Copyright   :  (c) Deivid Vale
--    License     :  MIT
--
--    Maintainer  :  deividrodriguesvale@gmail.com
--    Stability   :  experimental
--    Portability :  portable
--
--    Provide datatypes for simple types and an infer algorithm under a type environment.
module Polynomial.Pol where

{--------------------------------------------------------------------
  Datatypes
--------------------------------------------------------------------}

-- | Variable name in a polynomial representing the name and the expoent.
data Name = Name String Int
    deriving (Eq, Ord, Show)

data Coef = Coef Int String
    deriving (Eq, Ord, Show)

-- | A polynomial structure over Integers.
data Pol = Num Int | V Name | M Coef [Name] | Add Pol Pol | Mult Pol Pol | Exp Pol Int
    deriving (Eq, Ord, Show)


-- | Reduce polynomials to additions of monomials.
reduce :: Pol -> Pol

reduce (Num n) = Num n
reduce m@(M {}) = m

-- Rules for addition
reduce (Add (Num 0) p) = reduce p
reduce (Add p (Num 0)) = reduce p

-- Compatibiblity for Add
reduce (Add p q) = reduce $ Add (reduce p) ( reduce q)

-- Rules for Multiplication
-- Zero
reduce (Mult (Num 0) _) = Num 0
reduce (Mult _ (Num 0)) = Num 0

-- Unity
reduce (Mult (Num 1) p) = reduce p
reduce (Mult p (Num 1)) = reduce p

-- Monomial parameter multiplication
reduce (Mult (Num n) (M (Coef n' a) l)) = M (Coef (n * n') a) l

-- Distributivity
reduce (Mult p (Add q l)) = reduce $ Add (Mult p q) (Mult p l)

-- Compatibility for Mult
reduce (Mult p q) = reduce $ Mult (reduce p) (reduce q)
