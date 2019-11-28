module Type.SType where

    -- For now, simple types are just Haskell data types.
    data Type = Base String | Arrow Type Type
        deriving (Eq, Show)

    typeOrder :: Type -> Int
    typeOrder (Base _)      = 0
    typeOrder (Arrow x y)   = maximum [typeOrder x + 1, typeOrder y]
