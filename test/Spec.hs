{-# LANGUAGE OverloadedStrings #-}
import Data.Type.SType

import Data.Text

main :: IO ()
main = do
    putStrLn "Nothing to test yet."
    print (ack 10 3)

y = Sort "nat"
x = Arrow y (Arrow y y)

testPattern :: Type -> Bool
testPattern ty = case ty of
    Sort "x" -> True
    Arrow t t' -> False

ack :: Int -> Int -> Int
ack m n
    | m == 0 = n + 1
    | m > 0 && n == 0 = ack (m - 1) 1
    | m > 0 && n > 0 = ack (m - 1) (ack m (n - 1))
