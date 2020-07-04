module UnitTest.AFS where

import qualified Type.SimpleTypes              as ST
import qualified Term.AFS                      as AFS

assertAFS :: IO ()
assertAFS = do
    putStrLn "Creating natural number signature..."
    print nat
-- Create Symbol Types
nat = ST.newBasicType "nat"
natnat = ST.newArrowType nat nat

x = AFS.var "x"
zeroSignature = AFS.symbol "0" nat

s = AFS.symbol "s" natnat

zeroTerm = AFS.fApp zeroSignature []

sx = AFS.fApp s [x,x]

app = AFS.app x x

abst = AFS.abs x zeroTerm
