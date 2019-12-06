module Lib
    ( someFunc
    ) where

import qualified Type.SType as TYPE
import qualified Terms.ATerms as TERMS

someFunc :: IO ()
someFunc = print myvar
natType = TYPE.Base "nat"
natList = TYPE.Base "natlist"
arrow = TYPE.Arrow natList natList
arrow2 = TYPE.Arrow (TYPE.Arrow arrow natType) natType

myvar = TERMS.newVar "nome" natType

test = TYPE.typeOrder natType
