module Type.SType (
    Type(..),
    typeOrder,
    hugeArrow,
    TypeCheck(..)
) where
    import qualified Data.Set as Set

    -- | For now, simple types are just Haskell data types.
    data Type = Base String | Arrow Type Type
        deriving (Eq, Ord)

    instance Show Type where
        show (Base x) = x
        show (Arrow t1 t2) = show t1 ++ " => " ++ show t2

    typeOrder :: Type -> Int
    typeOrder (Base _)      = 0
    typeOrder (Arrow x y)   = maximum [typeOrder x + 1, typeOrder y]

    typeArity :: Type -> Int
    typeArity (Base _)    = 0
    typeArity (Arrow x _) = typeArity x + 1


    class TypeCheck exp where
        isValid  :: exp -> Bool
        axRule   :: exp -> Type
        -- appRule  :: exp -> exp -> Type

    -- | Takes a base type and produces an arrow type of size n using this type.
    hugeArrow :: Type -> Integer -> Type
    hugeArrow t 0 = t
    hugeArrow (Base name) n = hugeArrow (Arrow (Base name) (Base name)) (n - 1)
    hugeArrow (Arrow t1 t2) n = case t1 of
        Base name -> hugeArrow (Arrow (Base name) (Arrow t1 t2)) (n - 1)
