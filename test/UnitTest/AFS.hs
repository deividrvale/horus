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
    putStrLn "Creating natural numbers signature..."
    print nat
    print natnat
    print suc
    putStrLn "Printing some terms built with that signature..."
    print x
    print sucx
    print app
    print abst
    print abst2
    putStrLn "Declare axiomatic types."
    print xas
    print zeroAs

-- Basic Types Declaration
nat :: ST.Type

nat = ST.newBasicType "nat"
natnat :: ST.Type
natnat = ST.newArrowType nat nat

-- Signature Function Symbols
suc = AFS.symbol "suc" natnat

zeroSig = AFS.symbol "0" nat

-- Terms
x = AFS.var "x" -- a variable.
zero = AFS.const zeroSig

sucx = AFS.fApp suc [x, x]

app = AFS.app x x

abst = AFS.abs x x

abst2 = AFS.abs x abst

-- Declaring variables types
xas = ST.declareType x nat
zeroAs = ST.declareType zero nat
