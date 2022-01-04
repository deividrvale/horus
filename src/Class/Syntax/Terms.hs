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

module Class.Syntax.Terms where

import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Class.Type.SimpleType

class Terms t where
    -- | ´vars s´ returns the list of all free variables occuring in s.
    vars :: (Eq v, Ord v) => t v f -> Set.Set v
    pos  :: t v f -> IntSet.IntSet
    isVar :: t v f -> Bool
    isHO :: (AxTypable v, AxTypable f, TermTypable t) => t v f -> Bool
    -- | Injects a variable symbol into the term type.
    returnVar :: v -> t v f
