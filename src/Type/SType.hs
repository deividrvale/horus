module Type.SType where

    -- For now, simple types are just Haskell data types.
    data Type = Base String | Arrow Type Type

    -- Type class to define equality between basic types.
    class TypeEq a where
        isEqual :: a -> a -> Bool




    -- instance TypeEq Base where
    --     isEqual Base "" Base "" = True

    -- testType :: Type -> String
    -- testType (Base "") = "Tipo vazio"
    -- testType (Base zl) = "teste"
    -- testType (Arrow x y) = "x"
