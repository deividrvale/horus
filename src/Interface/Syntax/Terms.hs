{-|
Module      : Terms
Description : Interface for Terms
Copyright   : (c) Deivid Vale, 2021
License     : MIT
Maintainer  : deividrodriguesvale@gmail.com
Stability   : experimental
Portability : POSIX

Defines the interfacing for terms.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Interface.Syntax.Terms where

import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

class Terms t where
    -- | @vars s@ returns the list of all free variables occuring in s.
    vars :: (Eq v, Ord v) => t v f -> Set.Set v

    -- | @isVars s@ returns @True@ if @s@ is a variable
    -- and @False@, otherwise.
    isVar :: t v f -> Bool

    -- | Injects a variable value 'v' into a @t v f@ type constructed using 'v'.
    returnVar :: v -> t v f
