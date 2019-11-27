module Lib
    ( someFunc
    ) where

import Type.SType as TYPE

someFunc :: IO ()
someFunc = putStrLn "someFunc"

natType = TYPE.Base "nat"
natList = TYPE.Base "natlist"
arrow = TYPE.Arrow natList natList
arrow2 = TYPE.Arrow arrow arrow
