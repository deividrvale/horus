module UnitTest.AFS where

import qualified Type.SimpleTypes              as ST
import qualified Term.AFS                      as AFS

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

-- Basic Types Declaration
nat = ST.newBasicType "nat"
natnat = ST.newArrowType nat nat

-- Signature Function Symbols
suc = AFS.symbol "suc" natnat
zeroSignature = AFS.symbol "0" nat

-- Terms
x = AFS.var "x" -- a variable.
zeroTerm = AFS.fApp zeroSignature []

sucx = AFS.fApp suc [x,x]

app = AFS.app x x

abst = AFS.abs x x

abst2 = AFS.abs x abst
