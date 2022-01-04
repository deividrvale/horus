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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Data.AST.Expr where

import qualified Data.Set as Set
import Class.Syntax.Terms
import Class.Type.SimpleType
import Data.Type.SType

-- | This abstract type represent general expressions built using an AFS
-- (for now) syntax.
-- A Num constructor is added for Integer values.

data Expr v f =
    Num Int
    | Var v                       -- ^ Variables.
    | Fun f [Expr v f]           -- ^ Function application.
    | Abs v (Expr v f)           -- ^ Abstraction.
    | App (Expr v f) (Expr v f)  -- ^ Application.
    deriving (Eq, Show)

instance Terms Expr where
    vars t = case t of
        Num {} -> Set.empty
        Var v -> Set.singleton v
        Fun f [] -> Set.empty
        Fun _ args -> foldl Set.union Set.empty  (map vars args)
        Abs v t -> Set.difference (vars t) (Set.singleton v)
        App s t -> Set.union (vars s) (vars t)

    isVar (Var _) = True
    isVar _ = False

    returnVar v = Var v

instance TermTypable Expr where
    getType t = case t of
        Var x -> return $ getAxTy x
        Fun f [] -> return $ getAxTy f
        Fun f (s : args) -> undefined
        Abs x t -> mkArrow (getAxTy x) <$> getType t
        App s t -> do
            sTy <- getType s
            tTy <- getType t
            if isArr sTy -- Test if the type of an application is an arrow type.
                then do
                    lhs <- getLeft sTy
                    rhs <- getRight sTy
                    if lhs == tTy
                        then do
                            return rhs
                    else
                        Nothing
            else
                Nothing
