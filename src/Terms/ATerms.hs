module Terms.ATerms where
    import qualified Type.SType as TYPE

    -- data FSymbol = FArity String TYPE.Type
    -- deriving (Eq, Show)

    data Term = VarCons String Int TYPE.Type | AppCons Term Term
        deriving (Eq, Show)

    -- | Instantiate a new variable with the initial index.
    -- This is the eta-reduced version of the function, it returns a function
    newVar :: String -> TYPE.Type -> Term
    newVar name = VarCons name 1

    -- | Rename all variables in a term by it's index.
    renameVar :: Term -> Term
    renameVar var = case var of
        VarCons name i tp -> VarCons name (i + 1) tp
        AppCons t1 t2 -> AppCons (renameVar t1) (renameVar t2)
