import qualified UnitTest.AFS as AFS

main :: IO ()
main = do
    putStrLn "Executing Unit Test for AFS Terms. \n"
    AFS.assertAFS
