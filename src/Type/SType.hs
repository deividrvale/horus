-- |
--    Module      :  $Header$
--    Description :  Define simple type rules and type-checking.
--    Copyright   :  (c) Deivid Vale
--    License     :  MIT
--
--    Maintainer  :  deividrodriguesvale@gmail.com
--    Stability   :  experimental
--    Portability :  portable
--
--    Provide datatypes for simple types and an infer algorithm under a type environment.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Type.SType (
    -- * Types
    Type,
    TypeEnv,
    TypeEq,
    Infer,
    TypeError(..),

    -- * Abstraction: Types.
    isBasic,
    baseT,
    arrT,

    -- * Abstraction: Environment.
    empty,
    singleton,
    weaken,
    weakenL,
    remove,
    Type.SType.lookup,
    union,
    unionL,
    domain,
    fromList,
    toList,

    -- * Inference mechanism.
    inferType,
    inferTypeVerb,
    fresh,
    liftLookupEnv,
    liftLWeaken,

    -- * Class definition.
    Typable(..)
) where

import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.List as List

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

import AST.Subst
import Control.Monad.Reader

{--------------------------------------------------------------------
  Datatypes
--------------------------------------------------------------------}

-- | Type-variables.
type Name = Int

-- | Simple types.
data Type = Var Name | Base String | Arrow Type Type
    deriving (Eq, Show, Ord)

newtype TypeEnv exp = TypeEnv { assg :: Map.Map exp Type }
    deriving (Show, Eq)

-- | Equation between types.
type TypeEq = (Type, Type)

-- | Substitution between types.
type Subst = Map.Map Name Type

{--------------------------------------------------------------------
  Datatype Instances
--------------------------------------------------------------------}
instance Substitutable Subst Type where
    apply sub v@(Var x) = Map.findWithDefault v x sub
    apply _ (Base x) = Base x
    apply sub (Arrow a b) = Arrow (apply sub a) (apply sub b)

instance Substitutable Subst TypeEq where
    apply sub (l,r) = (apply sub l, apply sub r)

instance (Substitutable Subst a) => Substitutable Subst [a] where
    apply = map . apply

instance Substitutable Subst (TypeEnv Type) where
    apply sub (TypeEnv env) = TypeEnv $ Map.map (apply sub) env

instance Show TypeError where
    show (UnboundVariable x) = concat ["Variable ", x, " not in scope."]
    show (FunctionAppMismatch f) = concat ["Wrong number of arguments suplied to ", f, "."]
    show (UnificationError msg) = concat ["Typing Error: Cannot solve type constraint. Reason: ", msg]


{--------------------------------------------------------------------
  Abstraction Layer: Types
--------------------------------------------------------------------}
-- | The expression (isBasic t) returns True if t is a basic type.
isBasic :: Type -> Bool
isBasic (Base _) = True
isBasic _ = False

-- | Creates a base type.
baseT :: String -> Type
baseT = Base

-- | Instantiates a new arrow
arrT :: Type -> Type -> Type
arrT = Arrow

{--------------------------------------------------------------------
  Basic Operations on Types
--------------------------------------------------------------------}

-- | Get the set of type-variables occuring in a type.
var :: Type -> IntSet.IntSet
var t = case t of
    Var name -> IntSet.singleton name
    Base _ -> IntSet.empty
    Arrow l r -> IntSet.union (var l) (var r)

-- | Check if a name occurs in a type.
occurs :: Name -> Type -> Bool
occurs name tp = IntSet.member name (var tp)

{--------------------------------------------------------------------
  Abstraction Layer - Enviroment
--------------------------------------------------------------------}
-- | The Empty Environment.
empty :: TypeEnv exp
empty = TypeEnv Map.empty

-- | The singleton Environment.
singleton :: (Ord exp) => (exp, Type) -> TypeEnv exp
singleton (e, tp) = TypeEnv $ Map.singleton e tp

-- | Apply weakening to the type environment.
weaken :: (Ord exp) => TypeEnv exp -> (exp, Type) -> TypeEnv exp
weaken (TypeEnv env) (e, tp) = TypeEnv $ Map.insert e tp env

-- | Weakens the type environment by a finite number of assignments.
weakenL :: (Ord exp) => TypeEnv exp -> [(exp, Type)] -> TypeEnv exp
weakenL (TypeEnv env) es = TypeEnv $ Map.union (Map.fromList es) env

-- | Apply strengthening to the type enviroment.
remove :: (Ord exp) => TypeEnv exp -> exp -> TypeEnv exp
remove (TypeEnv env) e = TypeEnv $ Map.delete e env

-- | Return the type of an assigment.
lookup :: (Ord exp) => exp -> TypeEnv exp -> Maybe Type
lookup e (TypeEnv env) = Map.lookup e env

-- | Take the left-biased union of two env.
union :: (Ord exp) => TypeEnv exp -> TypeEnv exp -> TypeEnv exp
union (TypeEnv l) (TypeEnv r) = TypeEnv $ Map.union l r

-- | The the left-biased union of a finite number of Env.
unionL :: (Ord exp) => [TypeEnv exp] -> TypeEnv exp
unionL envs = List.foldl' union (TypeEnv Map.empty) envs

-- | Return the domain of the TypeEnv.
domain :: (Ord exp) => TypeEnv exp -> Set.Set exp
domain (TypeEnv env) = Set.fromList (Map.keys env)

-- | Instantiate a Type Environment from a list of assignments.
fromList :: (Ord exp) => [(exp, Type)] -> TypeEnv exp
fromList envs = TypeEnv $ Map.fromList envs

-- | Instantiate a Env. to a list of assingments.
toList :: (Ord exp) => TypeEnv exp -> [(exp, Type)]
toList (TypeEnv env) = Map.toList env

{--------------------------------------------------------------------
  Type Inference Monad
--------------------------------------------------------------------}
-- Using the inference monad (a RWST - Read Write State Transformer) we can
-- separate two diferent process on type checking.
-- Inference: generate a set of constraints to be solved by reversing
-- the typing derivation rules.
-- Constraint Solving: we get the generated constraints and solve them using unification.
-- After these two process we output a typerror a well-typed expression to
-- the expression evaluator.

-- | State of names used on type-inference.
data InferState = InferState { count :: Int }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0 }

data TypeError
  = UnboundVariable String
  | FunctionAppMismatch String
  | UnificationError String
  | UnificationMismatch [Type] [Type]


-- | The Inference monad
type Infer exp a = (ReaderT
                  (TypeEnv exp)   -- Typing environment
                  (StateT         -- Inference state
                      InferState
                      (Except     -- Inference errors
                      TypeError))
                  a)

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Generate a fresh name
fresh :: Infer exp Type
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ Var (count s + 1)

-- | Locally extend type environment to the Infer Monad
liftLWeaken :: (Ord exp) => (exp, Type) -> Infer exp a -> Infer exp a
liftLWeaken (x, sc) m = do
  let scope e = (remove e x) `weaken` (x, sc)
  local scope m

-- | Lookup type in the environment
liftLookupEnv :: (Show exp, Ord exp) => exp -> Infer exp Type
liftLookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
            Nothing ->  throwError $ UnboundVariable (show x)
            Just s  ->  return s

-- | Run the inference monad
runInfer :: TypeEnv exp -> Infer exp (Type, [TypeEq]) -> Either TypeError (Type, [TypeEq])
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

-- | Solve for the type of an expression in a given environment.
inferType :: Typable exp => TypeEnv exp -> exp -> Either TypeError Type
inferType env ex = case runInfer env (invRules ex) of
  Left err -> Left err
  Right (tp, unifPrb) -> case runUnify unifPrb of
    Left err -> Left err
    Right subst -> Right $ (apply subst tp)

-- | Verbosely solve for the type of an expression in a given environment.
inferTypeVerb :: Typable exp => TypeEnv exp -> exp -> Either TypeError ([TypeEq], Type, Subst)
inferTypeVerb env e = case runInfer env (invRules e) of
    Left err -> Left err
    Right (tp, unifPrb) -> case runUnify unifPrb of
        Left err -> Left err
        Right subst -> Right (unifPrb, tp, subst)


-------------------------------------------------------------------------------
-- Type Unification
-------------------------------------------------------------------------------

type UnifPrb = ([TypeEq], [TypeEq])

type Unif a = ExceptT TypeError Identity a

-- | Auxiliary (unifEq): instantiation rule.
-- Checks all conditions for applications of instantiation:
--  1. The variables need to be different to prevent nontermination.
--  2. Ocurrence check to prevent nontermination.
unifInst :: UnifPrb -> Unif UnifPrb
unifInst (prb, sol) = case prb of
    -- Equation between two variables.
    eq@(Var x, Var y) : ps -> if x == y then unify (ps, sol)
    else
        let sigma = Map.singleton x (Var y) in
            unify (apply sigma ps, eq : (apply sigma sol))
    -- Equation between a variable and a type.
    eq@(Var x, tp) : ps -> if x `occurs` tp then
        throwError $ UnificationError $ "Occurrence check error. The type-name " ++ show x ++ " occurs in " ++ show tp ++ "."
        else
            let sigma = Map.singleton x tp in
                unify (apply sigma ps, eq : (apply sigma sol))
    (_, _) : _ -> error "Fatal Error: Instantiation rule does not apply."

-- | Unification solver.
unify :: UnifPrb -> Unif UnifPrb
unify (prb, sol) = case prb of
    [] -> pure ([], sol)
    -- Symbol clash cases.
    (t1@(Base {}), t2@(Arrow {})) : ps ->
        throwError $ UnificationError $ "Symbol clash. Cannot unify " ++ show t1 ++ " with " ++ show t2 ++ "."
    (t1@(Arrow {}), t2@(Base {})) : ps ->
        throwError $ UnificationError $ "Symbol clash. Cannot unify " ++ show t1 ++ " with " ++ show t2 ++ "."
    (t1@(Base a), t2@(Base b)) : ps -> if a == b then unify (ps, sol) else
        throwError $ UnificationError  $ "Symbol clash. Cannot unify " ++ show t1 ++ " with " ++ show t2 ++ "."
    -- Instantiation rule.
    (Var {}, _) : _ -> unifInst (prb, sol)
    -- Orientation.
    -- NOTE: Orientation doesn't loop because Instantiation is applied before it.
    (tp, Var x) : ps -> unify ((Var x, tp) : ps, sol)
    -- Decompose.
    (l@(Arrow a b), r@(Arrow c d)) : ps -> if l == r then unify (ps, sol)
        else unify ((a,c) : (b,d) : ps, sol)

-- | Runs the unification procedure.
runUnify :: [TypeEq] -> Either TypeError Subst
runUnify prb = do
    (_, umg) <- runIdentity $ runExceptT $ unify (prb, [])
    return $ Map.fromList (map toSubst umg)
        where toSubst (Var x, tp) = (x, tp)

-------------------------------------------------------------------------------
-- Classes Definition
-------------------------------------------------------------------------------

class Typable exp where
    -- | Inverts the type-derivation rules returning the deducible type and a set of equations to be solved.
    invRules :: exp -> Infer exp (Type, [TypeEq])
