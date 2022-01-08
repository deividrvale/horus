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

module Data.AST.Expr (
    -- * Type
    Expr(..),

    module Interface.Syntax.Terms
) where

import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Prettyprinter as PP
import qualified Data.Text as T

import Interface.Syntax.Terms
import Data.Type.SType
import Data.Foldable ( Foldable(foldl') )

newtype Arity f ty = Ar (M.Map f ty)

-- | Type for expressions to support both algebraic (uncurried) functions
-- and curried symbols together with an abstraction and application constructor.
data Expr v f =
    Var v                        -- ^ Variables.
    | Sym f                      -- ^ d
    | Fun f [Expr v f]           -- ^ Function application.
    | Abs v (Expr v f)           -- ^ Abstraction.
    | App (Expr v f) (Expr v f)  -- ^ Application.
    deriving (Eq, Show)

instance Terms Expr where
    vars t = case t of
        Sym _ -> Set.empty
        Var v -> Set.singleton v
        Fun f [] -> Set.empty
        Fun _ args -> foldl' Set.union Set.empty  (map vars args)
        Abs v t -> Set.difference (vars t) (Set.singleton v)
        App s t -> Set.union (vars s) (vars t)

    isVar (Var _) = True
    isVar _ = False

    returnVar v = Var v

instance (AxSimplyTyped v, AxSimplyTyped f) => SimplyTyped (Expr v f) where
    getType t = case t of
        Var x -> return $ getAxTy x -- axiomatic typing
        Sym f -> return $ getAxTy f -- axiomatic typing
        Fun f [] -> return $ getAxTy f --axiomatic typing
        -- TODO: IMPORTANT: Implement this (it's not being using for AFS for now,
        -- future Deivid will hate me for my lazyness... well, who cares... ;)
        Fun f (s : args) -> undefined

        -- x :: a               t :: b
        -- ---------------------------
        --    (Abs x t) :: a -> b
        Abs x t -> Arrow (getAxTy x) <$> getType t

        -- s :: lhs -> rhs      t :: tTy
        -- -----------------------------
        --          (App s t) :: rhs
        App s t -> do
            sTy <- getType s -- the type of s, it must be an arrow type
            tTy <- getType t
            case sTy of
                Sort _ -> Nothing
                Arrow lhs rhs -> do
                    if lhs == tTy then
                        return rhs
                    else Nothing

instance (PP.Pretty v, PP.Pretty f) => PP.Pretty (Expr v f) where
    pretty e = case e of
        Var v -> PP.pretty v
        Sym f -> PP.pretty f
        Fun f args -> PP.pretty f PP.<> PP.tupled [PP.pretty ti | ti <- args]
        Abs v t -> PP.pretty ("λ" :: T.Text) PP.<> PP.pretty v PP.<> PP.parens (PP.pretty t)
        App s t -> PP.parens (PP.pretty s PP.<+> PP.pretty t)
