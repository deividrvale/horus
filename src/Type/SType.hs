module Type.SType where

    -- For now, simple types are just Haskell data types.
    data Type = Base String | Arrow Type Type
        deriving (Eq, Show)

    typeOrder :: Type -> Int
    typeOrder (Base _)      = 0
    typeOrder (Arrow x y)   = maximum [typeOrder x + 1, typeOrder y]

    typeArity :: Type -> Int
    typeArity (Base _)    = 0
    typeArity (Arrow x _) = typeArity x + 1

    -- -- A simple typechecking
    -- typeCheck :: (exp, Type) -> Bool
    -- typeCheck (exp, t) = case t of
    --     Base _ -> True
    --     Arrow t1 t2 ->

    appRule :: Type -> Type -> Maybe Type
    appRule (Base _x) _ = Nothing
    appRule (Arrow a b) x = if a == x then Just b else Nothing

    class TypeCheck exp where
        isValid  :: exp -> Bool
        axRule   :: exp -> Type
        -- appRule  :: exp -> exp -> Type
