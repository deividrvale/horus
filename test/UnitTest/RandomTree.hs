{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UnitTest.RandomTree where
import Control.Applicative
import Control.Lens ((??))
import Control.Monad.Random.Strict
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe ( fromJust )
import Data.List

data AddTerm = Z | S AddTerm | A AddTerm AddTerm

instance Show AddTerm where
    show Z = "0"
    show (S t) = "suc(" ++ show t ++ ")"
    show (A s t) = "add(" ++ show s ++ "," ++ show t ++ ")"
data Tree = Leaf | Branch Tree Tree
    deriving Show
newtype GenM a = GenM
    { unGenM :: ReaderT (Int, Int) (StateT Int (MaybeT (Rand StdGen))) a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadRandom, MonadState Int, MonadReader (Int, Int))

-- size :: Tree -> Int
-- size Leaf = 1
-- size (Branch l r) = 1 + size l + size r

size :: AddTerm -> Int
size t = case t of
    Z -> 1
    S t' -> 1 + size t'
    A t1 t2 -> size t1 + size t2

runGenM :: Int -> Double -> GenM a -> IO (Maybe a)
runGenM targetSize eps m = do
    let wiggle = floor $ fromIntegral targetSize * eps
        minSize = targetSize - wiggle
        maxSize = targetSize + wiggle
    g <- newStdGen
    return . (evalRand ?? g) . runMaybeT . (evalStateT ?? 0)
           . (runReaderT ?? (minSize, maxSize)) . unGenM
           $ m

-- chooseBranch :: Double -> AddTerm -> AddTerm
-- chooseBranch x
--     | x <= (1/2 :: Double) = \t -> Z
--     | x <= (3/4 :: Double) = \t -> S t
--     | x >  (3/4 :: Double) =

atom :: GenM ()
atom = do
    (_, maxSize) <- ask
    curSize <- get
    when (curSize >= maxSize) mzero
    put (curSize + 1)

genTreeUB :: GenM AddTerm
genTreeUB = do
    -- R is uniformly distributed
    r <- getRandom
    atom
    let choose | r >= (0.4 :: Double) = A <$> genTreeUB <*> genTreeUB
               | r > (0.2 :: Double) = S <$> genTreeUB
               | otherwise = return Z
    choose

genTreeLB :: GenM AddTerm
genTreeLB = do
    put 0
    t <- genTreeUB
    tSize <- get
    (minSize, _) <- ask
    guard $ tSize >= minSize
    return t

newGenTree :: GenM AddTerm
newGenTree = genTreeLB `mplus` newGenTree

--
assertRandomTree :: IO ()
assertRandomTree = do
    trees <- map show . fromJust <$> runGenM 15 0.1 (replicateM (10^5) newGenTree)
    print trees
