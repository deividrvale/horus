module Lib
    ( someFunc
    ) where

import qualified Type.SType as TYPE
import qualified Terms.ATerms as TERMS

someFunc :: IO ()
someFunc = print testbuild

nat = TYPE.Base "nat"
natList = TYPE.Base "natlist"
arrow = TYPE.Arrow natList natList
arrow2 = TYPE.Arrow (TYPE.Arrow arrow nat) nat

x = TERMS.newVar "Y" (TYPE.Arrow nat (TYPE.Arrow nat nat))
y = TERMS.newVar "X" nat

appTerm = TERMS.AppCons x y (TYPE.Arrow nat nat)

f = TERMS.FcSymbol "f" (TYPE.Arrow nat nat)
zero = TERMS.FcSymbol "0" nat

testTerm = TERMS.AppCons f zero (TYPE.Arrow nat nat)

hugeType = TYPE.hugeArrow nat (9^9)

functionOfHugeType = TERMS.FcSymbol "f" hugeType

testbuild = TERMS.buildApp functionOfHugeType
