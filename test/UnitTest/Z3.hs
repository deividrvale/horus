module UnitTest.Z3 where
import Data.Maybe
import Z3.Monad

script :: Z3 ()
script = do
    a <- mkFreshIntVar "a"
    b <- mkFreshIntVar "b"
    c <- mkFreshIntVar "c"
    _0 <- mkInteger 0
    assert =<< a `mkLt` b
    assert =<< c `mkGt` _0
    assert =<< a `mkGt` _0

checkSat :: Z3 Result
checkSat = do
    script
    check

returnModel :: Z3 (Maybe Model)
returnModel = do
    script
    p <- getModel
    return $ snd p

getValue :: Z3 (Maybe Integer)
getValue = do
    a <- mkFreshIntVar "a"
    b <- mkFreshIntVar "a"
    c <- mkFreshIntVar "a"
    _0 <- mkInteger 0
    assert =<< a `mkLt` b
    assert =<< c `mkGt` _0
    assert =<< a `mkGt` _0

    snd <$> (withModel $ (\m -> fromJust <$> evalInt m c))



printResult :: IO ()
printResult = do
    c <- evalZ3 checkSat
    m <- evalZ3 returnModel
    print c
    case m of
        Nothing -> putStr "There is no Model."
        Just model -> do
            m' <- evalZ3 $ modelToString model
            putStrLn $ concat ["The string of the model is\n", m']
    a <- evalZ3 getValue
    print a

