module UnitTest.Polynomial where

import Polynomial.Poly

assertPol :: IO ()
assertPol = do
    putStrLn "\nPolynomial Unit Test:"
    print poly
    print $ simplify poly

-- Very first examples of polynomials.
-- Create Variable names: X, Y
x = V 1 $ Name 1
y = V 1 $ Name 2
z = V 1 $ Name 3
-- Lets build some monomials for testing.
x2 = x `Exp` 2
y1 = y `Exp` 1

-- Examples of polynomials

-- P(X) = a1(X1 + a1)^2 * X2
-- poly = C 1 1 `Mult` ((x `Add` C 1 1) `Exp` 2) `Mult` y

-- Example Lankford paper
poly = linear 2 2
