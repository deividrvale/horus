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
{-# LANGUAGE BangPatterns #-}

module Polynomial.Poly where

{--------------------------------------------------------------------
  Datatypes
--------------------------------------------------------------------}
{-
    We realize polynomials as representations for functions
    f : N^n x ... x N^n -> N^m; where N^n appear k times, k >= 1.
    Note that each variable in the polynimal input is again a vector.
-}

newtype Name = Name Int
    deriving (Eq, Ord)

-- | A multi-vector polynomial structure over integers.
{- Constructors:
    Num builds elements of the domain.
    V builds a variable-vector. It encodes:
        - The projection coordinate.
        - Vector name.
    C builds a coefficient representation. It encodes:
        - Name of C
        - Multiplicity of C
-}
data Poly = Num !Int | V Int Name | C !Int !Int | Add !Poly !Poly | Mult !Poly !Poly | Exp !Poly !Int
    deriving (Eq, Ord)

infixl 9 `Exp`
infixr 8 `Mult`
infixr 7 `Add`

{--------------------------------------------------------------------
  Datatype Instances
--------------------------------------------------------------------}
instance Show Name where
    show (Name i) = "X" ++ show i

instance Show Poly where
    show (Num n) = show n
    show (V proj name) = show name ++ show proj
    show (C n m) = if m == 0 then show ""
        else if m == 1 then "a" ++ show n
            else concat [show m, "a", show n]
    show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Mult x y) = concat ["(", show x, " * ", show y, ")"]
    show (Exp p n) = case p of
        a@(Add _ _) ->  concat ["(", show a, ")", "^", show n]
        a@(Mult _ _) -> concat ["(", show a, ")", "^", show n]
        _ -> show p ++ "^" ++ show n

{--------------------------------------------------------------------
  Polynomial Reduction
--------------------------------------------------------------------}
-- We take an arbitrary polynomial and reduce it to a canonical form:
-- summation of monomials.

reduce :: Poly -> Poly
-- Normalized elements of reduce
reduce (Num i) = Num i
reduce (V proj name) = V proj name

-- Unit for addition
reduce (Add (Num 0) p) = p
reduce (Add p (Num 0)) = p
-- Unit for Mult
reduce (Mult (Num 1) p) = p
reduce (Mult p (Num 1)) = p
-- Unit for Exponential
reduce (Exp _ 0) = Num 1
reduce (Exp p 1) = p
reduce (Exp (Num 1) _) = Num 1
-- Zero element
reduce (Mult (Num 0) _) = Num 0
reduce (Mult _ (Num 0)) = Num 0

-- Rules for Add ----------------------------------------------------
reduce (Add (Num n) (Num m)) = Num (n + m)
-- Add for parameters
reduce p@(Add (C n1 m1) (C n2 m2)) =
    if n1 == n2 then C n1 (m1 + m2)
        else p

-- Defines right-associativy for Add
reduce (Add (Add p q) l) = Add p (Add q l)

-- Rules for Mult ---------------------------------------------------
-- Multiplication for numbers
reduce (Mult (Num n) (Num m)) = Num (n * m)
reduce (Mult x@(C n1 m1) y@(C n2 m2)) =
    if n1 == n2 then Num (m1 * m2) `Mult` (Exp (C n1 1) 2)
        else Mult x y
reduce (Mult (C n m) (Num k)) = C n (m * k)
reduce (Mult (Num k) (C n m)) = C n (m * k)
reduce (Mult x@(V _ n) y@(V _ m)) =
    if n == m then Exp x 2
        else Mult x y
-- Left-associativity for numbers
reduce (Mult (v@V {}) (Mult p q)) = Mult p (Mult v q)

-- Left-associativity for Mult
reduce (Mult p (Mult q l)) = Mult (Mult p q) l

-- Commutativity of Mult --------------------------------------------
reduce (Mult (V proj name) (Num n)) = Mult (Num n) (V proj name)


-- Distributivity
reduce (Mult p (Add q l)) = Add (Mult p q) (Mult p l)
reduce (Mult (Add q l) p) = Add (Mult q p) (Mult l p)

-- Rules for Exp ---------------------------------------------------
reduce (Exp (Exp x n) m) = Exp x (n * m)
reduce p@(Mult (Exp x n) (Exp y m)) =
    if x == y then Exp x (n + m) else p
-- 2-Binomial expansion
reduce (Exp (Add x y) 2) = Exp x 2 `Add` Num 2 `Mult` x `Mult` y `Add` Exp y 2
-- n-Binomial expansion
reduce (Exp (Add x y) n) = (Add x y) `Mult` Exp (Add x y) (n - 1)

-- Compatibility Rules ----------------------------------------------
-- Compatibility for Exp
reduce (Exp !p !n) = Exp (reduce p) n
-- Compatibility for Mult
reduce (Mult !p !q) = Mult (reduce p) (reduce q)
-- Compatibiblity for Add
reduce (Add !p !q) = Add (reduce p) (reduce q)
-- Fix-point for C
reduce (C n m) = C n m

-- | Applies a function until a fix-point
converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

-- | The expression `simplify p` returns a polynomial p' in expanded NF.
simplify :: Poly -> Poly
simplify = converge reduce

{--------------------------------------------------------------------
  Functions to create Polynomials
--------------------------------------------------------------------}

-- linear class
-- Represent the size of the tuples and the number of variables...
linear :: Int -> Int -> Poly

linear 0 0 = C 0 1

linear tuples vars = let i = tuples * vars in
    sum i vars where
        sum 0 _ = C 0 1
        sum tuples vars = C tuples 1 `Add` (sum (tuples - 1) vars)
