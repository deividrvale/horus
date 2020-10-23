-- |

module UnitTest.SimpleTypes where

import qualified Data.Map as Map
import qualified Data.Set as Set
import AST.Subst
import Type.SType
import qualified Term.AFS as AFS

assertST :: IO ()
assertST = do
    putStrLn "\nUnit Testing Curry ST"
    print $ inferType env expTerm

-- Base Types Declarations
nat = baseT "nat"
list = baseT "list"
a = baseT "A"

-- Basic functional type declarations
natnat = arrT nat nat

-- Signature Function Symbols
zero = AFS.const "zero" nat
suc = AFS.symbol "suc" [nat] nat
add = AFS.symbol "add" [nat, nat] nat
sigMap = AFS.symbol "map" [natnat, list] list

-- Terms
x = AFS.var "x"
y = AFS.var "y"
z = AFS.var "z"
sucx = AFS.fApp suc [x]
sucT = AFS.fApp suc [sucx]
fnT = AFS.abs (AFS.var "f") $ AFS.abs (AFS.var "q") (AFS.fApp sigMap [(AFS.var "f"), (AFS.var "q")])

abs1 = AFS.abs x sucx

-- Initial Type Environment
env :: TypeEnv AFS.Term
env = singleton (x, nat) `weaken` (y, nat) `weaken` (z, nat)

expTerm = fnT
