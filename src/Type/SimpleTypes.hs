{- |
    Module      :  $Header$
    Description :  Define simple types rules and type checking function and classes.
    Copyright   :  (c) Deivid Vale
    License     :  MIT

    Maintainer  :  deividrodriguesvale@gmail.com
    Stability   :  experimental
    Portability :  portable

    Defines simple types rules, type checking
-}
module Type.SimpleTypes (
     -- * Types
    Type,        -- instance Eq, Ord
    Assignment,
    Context,
    Judgment,

    -- * Abstraction: Type
    newVarType,
    newBasicType,
    newArrowType,
    isBasic,
    typeSubst,
    nameOccurs,

    -- * Abstraction: Assignment
    newAssignment,

    -- * Abstraction: Context
    emptyCtx,
    add,
    member,

    -- * Class
    SimpleTypedCurry(..)
) where
import qualified Data.Set as Set

-- | Type name.
-- Type variables are identified uniquely by their name.
type Name = Int

-- | Simple types.
data Type = Var Name | Base String | Arrow Type Type
    deriving (Eq, Ord)

instance Show Type where
    show (Var x) = show x
    show (Base name) = name
    show (Arrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

-- | An assigment
data Assignment term = Assignment term Type
    deriving (Eq, Ord)

instance (Show a) => Show (Assignment a) where
    show (Assignment a t) = show a ++ " :: " ++ show t

newtype Context exp = Context (Set.Set (Assignment exp) )
    deriving Show

data Judgment exp = Judgment (Context exp) (Assignment exp)

isBasic :: Type -> Bool
isBasic x = case x of
    (Base _) -> True
    (Arrow _ _) -> False

newVarType :: Int -> Type
newVarType = Var

newBasicType :: String -> Type
newBasicType = Base

newArrowType :: Type -> Type -> Type
newArrowType = Arrow

-- | Return type's order.
order :: Type -> Int
order (Base _)      = 0
order (Arrow x y)   = maximum [order x + 1, order y]

-- | Abstraction: Assignment
newAssignment :: (SimpleTypedCurry term) => term -> Type -> Assignment term
newAssignment = Assignment

-- | Abstraction: Context
emptyCtx :: Context exp
emptyCtx = Context Set.empty

-- | Add an assignment to a context.
add :: (SimpleTypedCurry term, Ord term) => Assignment term -> Context term -> Context term
add assg (Context ctx) = Context (Set.insert assg ctx)

-- | Check if a declaration is a member of a context.
member ::  (SimpleTypedCurry term, Ord term) => Assignment term -> Context term -> Bool
member asgn (Context ctx) = Set.member asgn ctx

-- | every simple typed term should implement this class.
class SimpleTypedCurry term where
    -- | Axiom function defines the most basic term structure such that the assignment t : Type is true.
    axiom :: Context term -> Assignment term -> Bool
    app :: Context term -> Assignment term -> Bool
    abs :: Context term -> Assignment term -> Bool
    declareType :: term -> Type -> Assignment term

-- Defining equations and dealing with type unification.
type TypeEq = (Type, Type)

data UnifPrb = Fail | UnifPrb [TypeEq]

-- Substitution on type names.
type Subst = [(Name, Type)]

-- | Check if the name occurs in the type t.
nameOccurs :: Name -> Type -> Bool
nameOccurs name tp = case tp of
    Var x -> x == name
    Base _ -> False
    Arrow tp1 tp2 -> (nameOccurs name tp1) || (nameOccurs name tp2)

-- | Check if the name n is on the substitution domain.
inDomain :: Name -> Subst -> Bool
inDomain name subst = case subst of
    [] -> False
    (x, _) : tl -> if x == name then True else inDomain name tl

-- | Apply a substitution to a type variable.
varSubst :: Name -> Subst -> Type
varSubst name subst = case subst of
    [] -> Var name
    (x, tp) : tl -> if x == name then tp else varSubst name tl

-- | Apply a substitution to a complete type.
-- TODO: Apply pure parallelism to Arrow argument.
typeSubst :: Type -> Subst -> Type
typeSubst tp sub = case tp of
    Var name -> if inDomain name sub then varSubst name sub else Var name
    Base b -> Base b
    Arrow l r -> Arrow (typeSubst l sub) (typeSubst r sub)

-- | Apply a substitution to a list of equations.
listSubst :: Subst -> [TypeEq] -> [TypeEq]
listSubst sub eq = case eq of
    [] -> []
    (tp1, tp2) : tl -> (typeSubst tp1 sub, typeSubst tp2 sub) : listSubst sub tl

-- | Solve a unification problem.
unifEq :: UnifPrb -> UnifPrb
unifEq Fail = Fail
unifEq (UnifPrb prb) = case prb of
    [] -> UnifPrb []
    -- (Var name, tp)
