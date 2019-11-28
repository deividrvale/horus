module Lib
    ( someFunc
    ) where

import qualified Type.SType as TYPE

someFunc :: IO ()
someFunc = print arrow

natType = TYPE.Base "nat"
natList = TYPE.Base "natlist"
arrow = TYPE.Arrow natList natList
arrow2 = TYPE.Arrow (TYPE.Arrow arrow natType) natType

test = TYPE.typeOrder natType

