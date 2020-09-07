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
import qualified Data.Set as Set

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
    putStrLn "Initializing new Type-Context:"
    print ctx
    print $ AFS.typeChecking ctx abst2 (ST.newVarType 1)
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
mapS = AFS.symbol "map" [natnat, nat] nat

ctx = ST.add zas $ ST.add yas (ST.initCtx xas)

-- Terms
x = AFS.var "x" -- a variable.
y = AFS.var "y"
z = AFS.var "z"
w = AFS.var "w"

sucx = AFS.fApp suc [x]
addxy = AFS.fApp add [abst,abst2]
doubleVar = AFS.abs x sucx

app = AFS.app x (AFS.app x y)

abst = AFS.abs (AFS.var "u") (AFS.var "u")

abst2 = AFS.abs x (AFS.abs y (AFS.app x y))

t1 = AFS.abs x (AFS.abs y (AFS.app x y))
t2 = AFS.abs z (AFS.app (AFS.abs x x) (AFS.abs y y))

betaT2 = AFS.abs z (AFS.abs y y)

mapT = AFS.fApp mapS [(AFS.var "f"), (AFS.var "l")]

omega = AFS.app (AFS.abs x (AFS.app x x)) (AFS.abs x (AFS.app x x))

-- Declaring variables types
xas = ST.declareType x nat
yas = ST.declareType y nat
zas = ST.declareType z list
