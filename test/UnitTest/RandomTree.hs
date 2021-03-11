{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UnitTest.RandomTree where
import Control.Applicative
import Control.Lens ((??), reuse)
import Control.Monad.Random.Strict
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe ( fromJust )
import Data.List

data AddTerm = Z | S AddTerm | A AddTerm AddTerm
    deriving Eq

instance Show AddTerm where
    show Z = "0"
    show (S t) = "s " ++ show t
    show (A s t) = "+ " ++ show s ++ " " ++ show t
newtype GenM a = GenM
    { unGenM :: ReaderT (Int, Int) (StateT Int (MaybeT (Rand StdGen))) a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadRandom, MonadState Int, MonadReader (Int, Int))

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

-- | Applies a function until a fix-point
converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

-- Apply add rules to add terms
reduce :: AddTerm -> AddTerm
reduce t = case t of
    -- Reduction
    A x Z -> reduce x
    A x (S y) -> reduce (S $ A x y)
    -- Compatibility
    Z -> Z
    S x -> S (reduce x)
    A x y -> A (reduce x) (reduce y)

normalize :: AddTerm -> AddTerm
normalize = converge reduce

dataFormat :: [AddTerm] -> [AddTerm] -> String
dataFormat lhs nf = case (lhs, nf) of
    ([],[]) -> ""
    (s : xs, n : ns) -> show s ++ "\t" ++ show n ++ "\n" ++ dataFormat xs ns

-- experimental function showing how to concatenate n data generations with decreasing sizes.
-- :-)
expFunction :: Int -> Int -> IO [AddTerm]
expFunction 0 _ = return []
expFunction size replicate = do
    (++) <$> (fromJust <$> runGenM size 0.509308127 (replicateM replicate newGenTree))  <*> expFunction (size - 1) replicate

assertRandomTree :: IO ()
assertRandomTree = do
    -- trees <- fromJust <$> runGenM 15 0.509308127 (replicateM (10^4) newGenTree)
    trees <- expFunction 20 (10^4)
    print "Data Generated."
    print "Normalizing elements of the dataset."
    let nTree = map normalize trees
    print "Saving Data to file."
    let formatString = dataFormat trees nTree
    writeFile "./data/rewriting.txt" formatString
    print "Data saved."
