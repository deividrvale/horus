import qualified UnitTest.AFS as AFS
import qualified UnitTest.SimpleTypes as ST

main :: IO ()
main = do
    AFS.assertAFS
    ST.assertST
