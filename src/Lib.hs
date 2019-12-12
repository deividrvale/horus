module Lib
    ( someFunc
    ) where

import qualified Type.SType as TYPE
import qualified Terms.ATerms as TERMS

someFunc :: IO ()
someFunc = print (TYPE.typeArity arrow2)

natType = TYPE.Base "nat"
natList = TYPE.Base "natlist"
arrow = TYPE.Arrow natList natList
arrow2 = TYPE.Arrow (TYPE.Arrow arrow natType) natType



x = TERMS.newVar "Y" (TYPE.Arrow natType (TYPE.Arrow natType natType))
y = TERMS.newVar "X" natType

appTerm = TERMS.AppCons x y (TYPE.Arrow natType natType)

f = TERMS.FcSymbol "f" (TYPE.Arrow natType (TYPE.Arrow natType natType))
zero = TERMS.FcSymbol "0" natType

testTerm = TERMS.AppCons f zero (TYPE.Arrow natType natType)

validTerm = TYPE.isValid testTerm
