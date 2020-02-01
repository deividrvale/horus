module Type.SType (
    Type(..),
    createSortSet,
    typeOrder,
    hugeArrow,
    TypeCheck(..)
) where
    import qualified Data.Set as Set

    -- | For now, simple types are just Haskell data types.
    data Type = Base String | Arrow Type Type
        deriving (Eq, Show, Ord)

    -- | A set S of sorts, this function is to be used when creating the set S from an user input file.
    -- | If you pass a type that is not base it just ignore it.
    createSortSet' :: [Type] -> Set.Set Type -> Set.Set Type
    createSortSet' [] set = set
    createSortSet' (hd : tl) set = case hd of
        (Base _) ->  createSortSet' tl (Set.insert hd set)
        (Arrow _ _) -> createSortSet' tl set

    createSortSet :: [Type] -> Set.Set Type
    createSortSet lst = createSortSet' lst Set.empty

    typeOrder :: Type -> Int
    typeOrder (Base _)      = 0
    typeOrder (Arrow x y)   = maximum [typeOrder x + 1, typeOrder y]

    typeArity :: Type -> Int
    typeArity (Base _)    = 0
    typeArity (Arrow x _) = typeArity x + 1

    appRule :: Type -> Type -> Maybe Type
    appRule (Base _x) _ = Nothing
    appRule (Arrow a b) x = if a == x then Just b else Nothing

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
