{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : W
Description : Short description
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Subst (
    Subst,

    fromList,
    Data.Subst.id,
    domain,
    getFunc,

    Substitutable(..)
) where

import qualified Data.Map as Map
import Interface.Syntax.Terms ( Terms(returnVar) )
import Prelude hiding (id)

newtype Subst v e = Subst { getMap :: Map.Map v e } deriving (Show, Eq)

fromList :: Ord v => [(v,e)] -> Subst v e
fromList l = Subst $ Map.fromList l

compose :: (Ord v, Substitutable e) => Subst v e -> Subst v e -> Subst v e
s1 `compose` s2 = Subst (Map.map (apply s1) (getMap s2) `Map.union` getMap s1)

id :: Subst v e
id = Subst Map.empty

domain :: Ord v => Subst v e -> [v]
domain (Subst m) = Map.keys m

getFunc :: (Ord v, Terms t) => Subst v (t v f)-> v -> t v f
getFunc (Subst m) v = case Map.lookup v m of
    Just e -> e
    Nothing -> returnVar v

class Substitutable e where
    apply :: Subst v e -> e -> e
    id `apply` s = s
