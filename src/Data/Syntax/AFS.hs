
{-|
Module      : Data.AFS
Description : Data structure for AFS.
Copyright   : (c) Deivid Vale
License     : MIT
Maintainer  : deividrodriguesvale@gmail.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Syntax.AFS (
    Term(..),

    pattern Var,
    pattern Fun,
    pattern App,
    pattern Lam,

    newLam
) where

import qualified Data.Text as T
import qualified Prettyprinter as PP

import qualified Data.AST.Expr as E
import Data.Type.SType

data V = V Int Type deriving (Eq, Show)

data F = F T.Text Type deriving (Eq, Show)

type Term = E.Expr V F

-- | Declare a new variable index with a type.
pattern Var :: Int -> Type -> Term
pattern Var i ty = E.Var (V i ty)

pattern Fun :: T.Text -> Type -> Term
pattern Fun name ty = E.Sym (F name ty)

pattern App :: Term -> Term -> Term
pattern App s t = E.App s t

pattern Lam :: V -> Term -> Term
pattern Lam v t = E.Abs v t

newLam :: Term -> Term -> Term
newLam x t = case x of
    Var i ty -> E.Abs (V i ty) t
    _ -> error "Fatal Error! The argument for the abstractor must be of kind variable type."

instance AxSimplyTyped V where
    getAxTy (V _ ty) = ty

instance AxSimplyTyped F where
    getAxTy (F _ ty) = ty

instance PP.Pretty V where
    pretty (V i _) = PP.pretty i

instance PP.Pretty F where
    pretty (F s _) = PP.pretty s

