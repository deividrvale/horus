module Lib
    ( someFunc
    ) where

import qualified Type.SType as Type
import qualified Term.ATerm as Term
import qualified Data.Set as SET
import qualified Rewriting.ARewriting as Rw
import qualified Interpretation.Polynomial as I

someFunc :: IO ()
someFunc = do
    -- putStrLn "Testing Arithmetic Rewriting . . ."
    -- putStr "Instantiating the base type set: "

    -- let baseSet = Type.createSortSet [nat]
    -- print baseSet

    -- putStr "Instantiating a signature set: "
    -- let sign = Term.createSignature [zero, succesor, plus, double, times]
    -- print sign

    putStrLn "Check the rule x + 0 -> x: "

    print (Type.typeOrder strangetype)

-- Base type
nat = Type.Base "nat"

-- Function Symbols
zero = Term.FcSymbol "0" nat
succesor = Term.FcSymbol "succ" (Type.Arrow nat nat)
plus = Term.FcSymbol "plus" (Type.Arrow nat (Type.Arrow nat nat))
double = Term.FcSymbol "double" (Type.Arrow nat nat)
times = Term.FcSymbol "times" (Type.Arrow nat (Type.Arrow nat nat))

-- Declare TRS variables
x = Term.VarCons "x" 0 nat
y = Term.VarCons "y" 0 nat

-- Lets create a single rule for R
l1 = Term.AppCons (Term.AppCons plus x (Type.Arrow nat nat)) zero nat
r1 = x

subst = [(x, Term.AppCons succesor x nat), (y, zero)]

testTerm = Term.AppCons x (Term.AppCons x x nat) nat

rule1 = Rw.isValidRule (l1,r1)

-- Sucessor of x and its interpretations to polynomials
sx = Term.AppCons succesor x nat

strangetype = Type.Arrow (Type.Arrow nat nat) (Type.Arrow (Type.Arrow nat nat) (Type.Arrow nat nat))
