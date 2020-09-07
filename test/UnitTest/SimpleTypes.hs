-- |

module UnitTest.SimpleTypes where

import qualified Type.SimpleTypes as ST

assertST :: IO ()
assertST = do
    putStrLn "\nUnit Testing Curry ST"

-- Basic types.
nat = ST.newBasicType "nat"
list = ST.newBasicType "list"

-- Types with variables.
var1 = ST.newVarType 1
var2 = ST.newVarType 2
arrow1 = ST.newArrowType var1 (ST.newVarType 1)
arrow2 = ST.newArrowType var1 list
