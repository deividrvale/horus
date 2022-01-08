{-|
Module      : AFS Terms
Description : Short description
Copyright   : (c) Deivid Vale, 2021
License     : MIT
Maintainer  : deividrodriguesvale@gmail.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Interface.Syntax.Terms where

import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

class Terms t where
    -- | ´vars s´ returns the list of all free variables occuring in s.
    vars :: (Eq v, Ord v) => t v f -> Set.Set v
    isVar :: t v f -> Bool
    returnVar :: v -> t v f
