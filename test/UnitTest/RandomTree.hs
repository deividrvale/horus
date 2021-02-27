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

data Tree = Leaf | Branch Tree Tree
    deriving Show

size :: Tree -> Int
size Leaf = 1
size (Branch l r) = 1 + size l + size r

-- Generates tree for training data.
randomTree' :: (RandomGen g) => Rand g Tree
randomTree' = do
    r <- getRandom
    if r < (1/2 :: Double)
        then return Leaf
        else Branch <$> randomTree' <*> randomTree'

generateTreeData :: (RandomGen g) => Int -> Rand g [Int]
generateTreeData n =
    reverse . sort . map size <$> replicateM n randomTree'

--
newtype GenM a = GenM
    { unGenM :: ReaderT (Int, Int) (StateT Int (MaybeT (Rand StdGen))) a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadRandom, MonadState Int, MonadReader (Int, Int))

runGenM :: Int -> Double -> GenM a -> IO (Maybe a)
runGenM targetSize eps m = do
    let wiggle = floor $ fromIntegral targetSize * eps
        minSize = targetSize - wiggle
        maxSize = targetSize + wiggle
    g <- newStdGen
    return . (evalRand ?? g) . runMaybeT . (evalStateT ?? 0)
           . (runReaderT ?? (minSize, maxSize)) . unGenM
           $ m

atom :: GenM ()
atom = do
    (_, maxSize) <- ask
    curSize <- get
    when (curSize >= maxSize) mzero
    put (curSize + 1)

genTreeUB :: GenM Tree
genTreeUB = do
    r <- getRandom
    atom
    if r <= (1/2 :: Double)
        then return Leaf
        else Branch <$> genTreeUB <*> genTreeUB

genTreeLB :: GenM Tree
genTreeLB = do
    put 0
    t <- genTreeUB
    tSize <- get
    (minSize, _) <- ask
    guard $ tSize >= minSize
    return t

newGenTree :: GenM Tree
newGenTree = genTreeLB `mplus` newGenTree

--
assertRandomTree :: IO ()
assertRandomTree = do
    putStrLn "\n Generating trees completly random."
    dumb <- evalRandIO (generateTreeData 100)
    print dumb
    putStrLn "\n Generating trees (n * log n) with (linearly) distrubuted sizes."
    smarter <- map size . fromJust <$> runGenM 100 0.1 (replicateM (10^4) newGenTree)
    print smarter
