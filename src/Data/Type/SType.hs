{-|
Module      : Simple Type
Description : Short description
Copyright   : (c) Deivid Vale, 2021
License     : MIT
Maintainer  : deividrodriguesvale@gmail.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
{-# LANGUAGE PatternSynonyms, OverloadedStrings #-}

module Data.Type.SType (
    Type,
    pattern Sort,
    pattern Arrow,
    module Class.Type.SimpleType
) where

import Class.Type.SimpleType
import Data.Text

{--------------------------------------------------------------------
  Datatypes
--------------------------------------------------------------------}

data Type =
    Base Text
    | Arr Type Type
    deriving (Eq, Show, Ord)

pattern Sort :: Text -> Type
pattern Sort x = Base x

pattern Arrow :: Type -> Type -> Type
pattern Arrow t t' = Arr t t'

{--------------------------------------------------------------------
    Classes Instances
--------------------------------------------------------------------}

instance SType Type where
    order ty = case ty of
        Base o -> 0
        Arr st st' -> max (order st + 1) (order st')

    base ty = case ty of
        b@(Base _) -> [b]
        Arr st st' -> base st ++ base st'

    isBase (Base _) = True
    isBase Arr {} = False

    isArr (Base _) = False
    isArr Arr {} = True

