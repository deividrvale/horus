-- |
--    Module      :  $Header$
--    Description :  Test unit for AFS terms.
--    Copyright   :  (c) Deivid Vale
--    License     :  MIT
--
--    Maintainer  :  deividrodriguesvale@gmail.com
--    Stability   :  experimental
--    Portability :  portable
--
--    Defines basic tests units for AFS terms.

module UnitTest.AFS where

import qualified Type.SimpleTypes as ST
import qualified Term.AFS as AFS

assertAFS :: IO ()
assertAFS = do
    putStrLn "\nTesting AFS Terms"
    putStrLn "\nDeclaring base types:"
    print nat
    print list
    putStrLn "Declaring signature symbols:"
    print zero
    print suc
    print add
    print addxy -- test finding a type for this term.
    -- putStr "Initialing new Type Context:"
    -- print ctx
    -- putStrLn "Generating experimental type equations."
    -- eq <- return $ AFS.genTypeEq ctx abst2 (ST.newVarType 1)
    -- print eq
    -- putStrLn "Solving equations using unification."
    -- print $ ST.solveEq (genUnif eq)

-- Base Types Declarations
nat = ST.newBasicType "nat"
list = ST.newBasicType "list"
a = ST.newBasicType "A"

-- Basic functional type declarations
natnat = ST.newArrowType nat nat

-- Signature Function Symbols
zero = AFS.const "zero" nat
suc = AFS.symbol "suc" [nat] nat
add = AFS.symbol "add" [nat, nat] nat

-- Empty Context.
ctx = ST.add yas (ST.initCtx xas)

-- Terms
x = AFS.var "x" -- a variable.
y = AFS.var "y"
z = AFS.var "z"

sucx = AFS.fApp suc [x]
addxy = AFS.fApp add [abst,abst2]
doubleVar = AFS.abs x sucx

app = AFS.app x (AFS.app x y)

abst = AFS.abs (AFS.var "u") (AFS.var "u")

abst2 = AFS.abs x (AFS.abs y x)

-- Declaring variables types
xas = ST.declareType x nat
yas = ST.declareType y nat

-- | Get a list of equations and generate a unification problem to be solved.
genUnif :: Maybe [(ST.Type, ST.Type)] -> ST.UnifPrb
genUnif meq = case meq of
    Nothing -> ST.Fail
    Just eqs -> ST.UnifPrb eqs []
