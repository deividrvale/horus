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
import Data.Syntax.AFS as AFS

import qualified Data.Text as T
import Data.Type.SType
import qualified Prettyprinter as PP
import Prettyprinter.Render.Text
import Prettyprinter.Util
import System.IO
import Data.Syntax.AFS (newLam)

assertAFS :: IO ()
assertAFS = do
  putStrLn "\nTesting AFS Terms ---------------------------"
--   putDocW 32 (reflow "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
--   hbuffering <- (hGetBuffering stdout)
--   print hbuffering
--   renderIO System.IO.stdout (PP.layoutPretty PP.defaultLayoutOptions "ab")
  putDoc (PP.pretty natNat)


nat = Sort "n"
natNat = Arrow nat (nat `Arrow` nat)

x :: Term
x = Var 1 (nat `Arrow` nat)

s = Fun "s" (nat `Arrow` (nat `Arrow` nat))



s0 :: Term
s0 = App s x

l = newLam x s0
