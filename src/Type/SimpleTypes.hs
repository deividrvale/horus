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
    Type,
    Assignment(..),
    Context(..),
    Judgment(..),
    newBasicType,
    newArrowType,
    isBasic,
    SimpleTypedCurry(..)
) where
    import qualified Data.Set as Set

    -- | Simple types.
    data Type = Base String | Arrow Type Type
        deriving (Eq, Ord)

    instance Show Type where
        show (Base name) = name
        show (Arrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"


    -- | Typing contexts...

    data Assignment exp = Assignment exp Type
        deriving Eq

    instance (Show a) => Show (Assignment a) where
        show (Assignment a t) = show a ++ " :: " ++ show t


    newtype Context exp = Context (Set.Set (Assignment exp) )
        deriving Show

    data Judgment exp = Judgment (Context exp) (Assignment exp)


    isBasic :: Type -> Bool
    isBasic x = case x of
        (Base _) -> True
        (Arrow _ _) -> False

    newBasicType :: String -> Type
    newBasicType = Base

    newArrowType :: Type -> Type -> Type
    newArrowType = Arrow


    nArrow :: Type -> Integer -> Type
    nArrow t 0 = t
    nArrow t n = nArrow (newArrowType t t) (n - 1)




    -- Simple types with context rule implementations
    axiomRule :: (SimpleTypedCurry term) => term -> term
    axiomRule = undefined

    -- | Return type's order.
    order :: Type -> Int
    order (Base _)      = 0
    order (Arrow x y)   = maximum [order x + 1, order y]

    -- | Every simple typed term should implement this class.
    class SimpleTypedCurry term where
        -- | Axiom function defines the most basic term structure such that the assignment t : Type is true.
        axiom :: Assignment term -> Bool
        assignType :: term -> Type -> Assignment term
        insertIntoContext :: Assignment term -> Context term
