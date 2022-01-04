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

{-# LANGUAGE OverloadedStrings #-}

module Class.Type.SimpleType where
import Data.Text

class (Eq t) => SType t where
    mkSort :: Text -> t
    mkArrow :: t -> t -> t
    getLeft :: t -> Maybe t
    getRight :: t -> Maybe t
    order  :: t -> Int
    base   :: t -> [t]
    isBase :: t -> Bool
    isArr  :: t -> Bool

-- | Define the class of that can be typable.

class TermTypable t where
    -- | Check the type of an AST expression.
    -- It requires axiomatic type-structure for variables and function symbols.
    check :: (AxTypable v, AxTypable f) => t v f -> Bool

    -- | Get the type of an AST expression.
    -- It requires axiomatic type-structure for variables and function symbols.
    getType :: (AxTypable v, AxTypable f, SType ty) => t v f -> Maybe ty


{-| D
-}
class AxTypable e where
    getAxTy :: SType ty => e -> ty
