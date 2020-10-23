-- |
--    Module      :  $Header$
--    Description :  Test unit for AFS terms.
--    Copyright   :  (c) Deivid Vale
--    License     :  MIT
--
--    Maintainer  :  deividrodriguesvale@gmail.com
--    Stability   :  experimental
--    Portability :  portable
--
--    Defines basic tests units for AFS terms.
module UnitTest.AFS where

import qualified Term.AFS as AFS

assertAFS :: IO ()
assertAFS = do
  putStrLn "\nTesting AFS Terms ---------------------------"
