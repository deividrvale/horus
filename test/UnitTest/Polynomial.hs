module UnitTest.Polynomial where

import Polynomial.Pol

assertPol :: IO ()
assertPol = do
    putStrLn "\nPolynomial Unit Test:"
    print pol1
    -- print pol2
    print $ reduce pol1

-- Very first examples of polynomials.
-- Create Variable names: X, Y
x = Name "X" 2
y = Name "Y" 1

-- Lets build some monomials for testing.
x2 = M (Coef 1 "") [x]
y1 = M (Coef 1 "") [y]

-- Examples of polynomials
-- P(X) = 7X + 1 + 3
-- pol1 = (Num 1) `Mult` ((M (Coef 7 "") [x, y] `Add` Num 0) `Add` (Num 0 `Mult` x2))
pol1 = (Num 1 `Add` x2) `Mult` Num 2

-- P(X,Y) = X^2(Y + 1)
pol2 = Num 0 `Mult` (y1 `Add` Num 1)

pol3 = pol1 `Mult` pol2
