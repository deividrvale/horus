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

module Data.Type.SType (
    Type,
    pattern Sort,
    pattern Arrow,

    AxSimplyTyped(..),
    SimplyTyped(..)
) where

import qualified Prettyprinter as PP
import qualified Data.Text as T

{--------------------------------------------------------------------
  Datatypes
--------------------------------------------------------------------}

data Type =
    Base T.Text
    | Arr Type Type
    deriving (Eq, Show, Ord)

pattern Sort :: T.Text -> Type
pattern Sort x = Base x

pattern Arrow :: Type -> Type -> Type
pattern Arrow t t' = Arr t t'

class AxSimplyTyped e where
    getAxTy :: e -> Type

class SimplyTyped e where
    getType :: e -> Maybe Type
    checkType :: e -> Bool
    checkType e = case getType e of
        Nothing -> False
        Just ty -> True


{--------------------------------------------------------------------
    Classes Instances
--------------------------------------------------------------------}
instance PP.Pretty Type where
    pretty = pp id where
        pp _ (Base name) = PP.pretty name
        pp paren (Arrow ty1 ty2) = paren (
            pp PP.parens ty1 PP.<+> PP.pretty ("->":: T.Text) PP.<+> pp id ty2
            )
