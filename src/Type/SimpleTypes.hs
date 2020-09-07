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
--    Provide simple type rules and type checking.

module Type.SimpleTypes (
     -- * Types
    Type,        -- instance Eq, Ord
    Assignment,
    Context,
    Judgment,
    TypeEq,
    UnifPrb,

    -- * Abstraction: Type
    newVarType,
    freshTypeVar,
    newBasicType,
    newArrowType,
    var,
    isBasic,
    order,
    nameOccurs,
    fresh,
    typeSubst,

    -- * Abstraction: Unification
    solveEq,
    newUnif,

    -- * Abstraction: Assignment
    newAssignment,

    -- * Abstraction: Context
    initCtx,
    add,
    member,
    domain,
    getType,
    shiftCtx,

    -- * Class
    SimpleTypedCurry(..)
) where

import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

{--------------------------------------------------------------------
  Datatypes
--------------------------------------------------------------------}

-- | Type-name. Simple type variables are identified uniquely by their name.
type Name = Int

-- | Simple types.
data Type = Var Name | Base String | Arrow Type Type
    deriving (Eq, Ord)

-- | An assigment
data Assignment term = Assignment term Type
    deriving (Eq, Ord)

-- | Type-equation.
type TypeEq = (Type, Type)

data Context exp = Empty | Context (Set.Set (Assignment exp) )
    deriving Show

data Judgment exp = Judgment (Context exp) (Assignment exp)

-- | Unification problem.
-- TODO: Re-implement this type internally as Maybe ([TypeEq], TypeEq).
-- So I can use them as a monad.
data UnifPrb = Fail | UnifPrb [TypeEq] [TypeEq]
    deriving (Show)

-- | Substitution.
type Subst = [(Name, Type)]

{--------------------------------------------------------------------
  Datatype Instances
--------------------------------------------------------------------}

instance Show Type where
    show (Var x) = show x
    show (Base name) = name
    show (Arrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

instance (Show a) => Show (Assignment a) where
    show (Assignment a t) = show a ++ " :: " ++ show t

{--------------------------------------------------------------------
  Datatype Abstraction Layer
--------------------------------------------------------------------}

-- | Test if a type is basic.
isBasic :: Type -> Bool
isBasic x = case x of
    (Base _) -> True
    (Arrow _ _) -> False

-- | Instantiate a new type-variable.
newVarType :: Int -> Type
newVarType = Var

-- | Instantiate a new base-type.
newBasicType :: String -> Type
newBasicType = Base

-- | Instantiate a new arrow-type.
newArrowType :: Type -> Type -> Type
newArrowType = Arrow

-- | Instantiate a new assignment
newAssignment :: (SimpleTypedCurry term) => term -> Type -> Assignment term
newAssignment = Assignment

-- | Instantiate a new type-equation.
newTypeEq :: Type -> Type -> TypeEq
newTypeEq t1 t2 = (t1, t2)

-- | Instantiate a new unification problem over types.
newUnif :: Maybe [(Type, Type)] -> UnifPrb
newUnif mEq = case mEq of
    Nothing -> Fail
    Just eq' -> UnifPrb eq' []

{--------------------------------------------------------------------
  Basic Operations on Types
--------------------------------------------------------------------}

-- | Get the set of type-variables occuring in a type.
var :: Type -> IntSet.IntSet
var t = case t of
    Var name -> IntSet.singleton name
    Base _ -> IntSet.empty
    Arrow l r -> IntSet.union (var l) (var r)

-- | Generate a new fresh type variable.
freshTypeVar :: Type -> Type
freshTypeVar t = if IntSet.null (var t) then Var 1
    else Var ((IntSet.findMax $ var t) + 1)

-- | Shift over the indices of a type.
shiftTp :: Type -> Int
shiftTp tp = if not (IntSet.null $ var tp) then
    (IntSet.findMax $ var tp) + 1
    else 1

-- | Return type's order.
order :: Type -> Int
order (Base _)      = 0
order (Arrow x y)   = maximum [order x + 1, order y]

-- | Check if a name occurs in a type.
nameOccurs :: Name -> Type -> Bool
nameOccurs name tp = IntSet.member name (var tp)

-- | Under a context and type, generate fresh type variable occuring anywhere.
fresh :: Ord term => Context term -> Type -> Type
fresh ctx tp = Var (shiftCtx ctx + shiftTp tp)

{--------------------------------------------------------------------
  Basic Operations on Contexts
--------------------------------------------------------------------}

-- | Instantiate an empty context.
initCtx :: Ord term => Assignment term -> Context term
initCtx assg = Context (Set.singleton assg)

-- | Return a context's domain.
domain :: Ord term => Context term -> Set.Set term
domain (Context ctx) = let ctx' = Set.toList ctx in
    case ctx' of
        [] -> Set.empty
        asg@(Assignment t tp) : tl -> Set.singleton t `Set.union` domain ctx''
            where ctx'' = Context (Set.fromList tl)

-- | Shift context indices on type variables.
-- Sum over all indices occuring in t.
shiftCtx :: Ord term => Context term -> Int
shiftCtx (Context ctx) = let ctx' = Set.toList ctx in
    foldl (+) 1 (map getIndex ctx')
    where getIndex (Assignment _ tp) = case tp of
            Var i  -> i
            Base _ -> 1
            Arrow a b -> (IntSet.findMax $ var a) + (IntSet.findMax $ var b)


-- | Add an assignment to a context.
add :: (SimpleTypedCurry term, Ord term) => Assignment term -> Context term -> Context term
add assg (Context ctx) = Context (Set.insert assg ctx)

-- | Check if a declaration is a member of a context.
member ::  (SimpleTypedCurry term, Ord term) => Assignment term -> Context term -> Bool
member asgn (Context ctx) = Set.member asgn ctx

-- Auxiliary functions for getType ----------------------------------
-- | Auxiliary: Check if an expression is part of an assignment.
isEq :: (SimpleTypedCurry term, Eq term) => term -> Assignment term -> Bool
isEq term (Assignment var tp) = term == var

-- | Auxiliary: Return the type in an single-element list of assignments.
returnType :: [Assignment term] -> Maybe Type
returnType [] = Nothing
returnType ((Assignment _ tp) : []) = Just tp
-- ------------------------------------------------------------------

-- | Get the type of a variable in a context.
getType :: (SimpleTypedCurry term, Ord term, Eq term) => Context term -> term -> Maybe Type
getType (Context ctx) t = returnType (Set.toList $ Set.filter (isEq t) ctx)

{--------------------------------------------------------------------
  Basic Operations with Substitutions.
--------------------------------------------------------------------}

-- | Check if a name is in a substitution domain.
inDomain :: Name -> Subst -> Bool
inDomain name subst = case subst of
    [] -> False
    (x, _) : tl -> if x == name then True else inDomain name tl

-- | Homomorphically extend substitution to Type.
-- TODO: Apply pure parallelism to Arrow argument.
typeSubst :: Type -> Subst -> Type
typeSubst tp sub = case tp of
    Var name -> if inDomain name sub then baseSubst name sub else Var name
    Base b -> Base b
    Arrow l r -> Arrow (typeSubst l sub) (typeSubst r sub)
    where baseSubst x ts = case ts of
            [] -> Var x
            (v, tp) : tl -> if x == v then tp else baseSubst x tl

-- | Extend substitution application to equations.
eqSubst :: Subst -> TypeEq -> TypeEq
eqSubst sub (l,r) = (typeSubst l sub, typeSubst r sub)

-- | Extend substitution application to list of equations.
listSubst :: [TypeEq] -> Subst -> [TypeEq]
listSubst eq sub = map (eqSubst sub) eq

{--------------------------------------------------------------------
  Unification on Types.
--------------------------------------------------------------------}

-- | Auxiliary (unifEq): instantiation rule. Only used in unifEq.
-- This function checks all conditions for application of instantiation:
--  1. The variables needs to be diferent to prevent nontermination.
--  2. Ocurrence check to prevent nontermination.
unifInst :: UnifPrb -> UnifPrb
unifInst (UnifPrb prb sol) = case prb of
    eq@(Var x, Var y) : tl -> if x == y then UnifPrb tl sol
        else unifEq $ UnifPrb (listSubst tl [(x, Var y)]) ( eq : (listSubst sol [(x, Var y)]))
    eq@(Var name, tp) : tl -> if nameOccurs name tp then Fail
        else unifEq $ UnifPrb (listSubst tl [(name, tp)]) (eq : (listSubst sol [(name, tp)]))
    -- Instantiation rule only apply when the equation's lhs is a variable.
    (_, _) : tl -> error "Fatal Error: Instantiation rule does not apply."

-- | Solve a unification problem.
unifEq :: UnifPrb -> UnifPrb
unifEq Fail = Fail
unifEq unif@(UnifPrb prb sol) = case prb of
    [] -> UnifPrb [] sol
    -- Symbol clash.
    (Base _, Arrow _ _) : tl -> Fail
    (Base a, Base b) : tl -> if a == b then unifEq (UnifPrb tl sol) else Fail
    (Arrow _ _, Base _) : tl -> Fail
    -- Instantiation.
    (Var name, _) : tl -> unifEq (unifInst unif)
    -- Orient.
    -- NOTE: Orient does not loop because Instantiation is applied before it.
    (tp, Var name) : tl -> unifEq $ UnifPrb ((Var name, tp) : tl) sol
    -- Decompose.
    (l@(Arrow a b), r@(Arrow c d)) : tl -> if l == r then unifEq (UnifPrb tl sol)
        else unifEq $ UnifPrb ((a,c) : (b,d) : tl) sol

-- | Wrapper for solving equations over types.
-- This function insert solutions in the Maybe monad.
solveEq :: Maybe [TypeEq] -> Maybe Subst
solveEq xs = let prb = newUnif xs in
    case unifEq prb of
        Fail -> Nothing
        UnifPrb [] umg -> pure (map toSubst) <*> Just umg
            where toSubst (Var name, tp) =  (name, tp)
        UnifPrb _ _ -> error "Fatal Error: Unification procedure fails. Please check the unification implementation on SimpleTypes module."

{--------------------------------------------------------------------
  Class defining Symple Types with Curry typing.
--------------------------------------------------------------------}

-- | Every simple typed term should implement this class.
class SimpleTypedCurry term where
    axiom :: Context term -> Assignment term -> Bool
    declareType :: term -> Type -> Assignment term
    typeChecking :: Context term -> term -> Type -> Either Bool Type
