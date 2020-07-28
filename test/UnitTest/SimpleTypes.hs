-- |

module UnitTest.SimpleTypes where

import qualified Type.SimpleTypes as ST

assertST :: IO ()
assertST = do
    putStrLn "\n Unit Testing Curry ST"
    print arrowVar
    print nType
    print (ST.nameOccurs 1 arrowVar)

nat = ST.newBasicType "nat"
list = ST.newBasicType "list"
var1 = ST.newVarType 1

arrowVar = ST.newArrowType var1 list

subst = [(1, arrowVar)]

nType = ST.typeSubst arrowVar subst
