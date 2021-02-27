{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UnitTest.RandomTree where
import Control.Applicative
import Control.Monad.Random.Strict
import Data.List

data Tree = Leaf | Branch Tree Tree
    deriving Show

size :: Tree -> Int
size Leaf = 1
size (Branch l r) = 1 + size l + size r

randomTree :: (Applicative m, MonadRandom m) => m Tree
randomTree = do
    r <- getRandom
    if r < (1/2 :: Double)
        then return Leaf
        else Branch <$> randomTree <*> randomTree

-- generateTrees = reverse . sort . map size <$> replicateM 100 randomTree

-- Generates tree for training data.
randomTree' :: (RandomGen g) => Rand g Tree
randomTree' = do
    r <- getRandom
    if r < (1/2 :: Double)
        then return Leaf
        else Branch <$> randomTree <*> randomTree

generateTreeData :: (RandomGen g) => Int -> Rand g [Int]
generateTreeData n =
    reverse . sort . map size <$> replicateM n randomTree'

throwDice :: (RandomGen g) => Rand g Int
throwDice = getRandomR (1, 6)

dice :: (RandomGen g) => Int -> Rand g [Int]
dice n = replicateM n throwDice

assertRandomTree :: IO ()
assertRandomTree = do
    values <- evalRandIO (generateTreeData (10^2))
    print values
