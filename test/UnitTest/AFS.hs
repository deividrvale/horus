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
    putStr "Initialing new Type Context:"
    print ctx
    putStrLn "Generating experimental type equations."
    eq <- return $ AFS.genTypeEq ctx abst a
    print eq


-- Base Types Declarations
nat = ST.newBasicType "nat"
list = ST.newBasicType "list"
a = ST.newBasicType "A"

-- Signature Function Symbols
zero = AFS.symbol "0" nat
suc = AFS.symbol "suc" (ST.newArrowType nat nat)
add = AFS.symbol "add" (ST.newArrowType nat (ST.newArrowType nat nat))

-- Empty Context.
ctx = ST.add yas (ST.initCtx xas)

-- Terms
x = AFS.var "x" -- a variable.
y = AFS.var "y"
z = AFS.var "z"
zeroTerm = AFS.const zero

sucx = AFS.fApp suc [x, x]

app = AFS.app x (AFS.app x y)

abst = AFS.abs (AFS.var "u") (AFS.var "u")

abst2 = AFS.abs x abst

-- Declaring variables types
xas = ST.declareType x (ST.newArrowType nat nat)
yas = ST.declareType y nat
zeroAs = ST.declareType zeroTerm nat
