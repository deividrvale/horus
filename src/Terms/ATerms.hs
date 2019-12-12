module Terms.ATerms where
    import qualified Type.SType as TYPE

    -- data FSymbol = FArity String TYPE.Type
    -- deriving (Eq, Show)

    data Term = VarCons String Int TYPE.Type | FcSymbol String TYPE.Type | AppCons Term Term TYPE.Type
        deriving (Eq, Show)

    -- data TipoTeste (x:: int) = Cons1 String x | Cons2 (TipoTeste x)

    -- | Instantiate a new variable with the initial index.
    -- This is the eta-reduced version of the function, it returns a function
    newVar :: String -> TYPE.Type -> Term
    newVar name = VarCons name 0

    -- | Rename all variables in a term by increasing it's index.
    renameVar :: Term -> Term
    renameVar var = case var of
        VarCons name i tp -> VarCons name (i + 1) tp
        FcSymbol name tp -> FcSymbol name tp
        AppCons s1 s2 tp -> AppCons (renameVar s1) (renameVar s2) tp


    instance TYPE.TypeCheck Term where
        isValid (VarCons _ _ _) = True
        isValid (FcSymbol _ _)  = True
        isValid (AppCons lhs rhs appType) = case lhs of
            VarCons _ _ (TYPE.Base _) -> False
            VarCons _ _ (TYPE.Arrow a b) -> case rhs of
                VarCons _ _ t2Type -> a == t2Type && appType == b
                AppCons _ _ t2Type -> a == t2Type && appType == b
                FcSymbol _ t2Type  -> a == t2Type && appType == b

            FcSymbol _ (TYPE.Base _) -> False
            FcSymbol _ (TYPE.Arrow a b) -> case rhs of
                VarCons _ _ t2Type -> a == t2Type && appType == b
                AppCons _ _ t2Type -> a == t2Type && appType == b
                FcSymbol _ t2Type  -> a == t2Type && appType == b

            AppCons _ _ (TYPE.Base _) -> False
            AppCons _ _ (TYPE.Arrow a b) -> case rhs of
                VarCons _ _ t2Type -> a == t2Type && appType == b
                AppCons _ _ t2Type -> a == t2Type && appType == b
                FcSymbol _ t2Type  -> a == t2Type && appType == b

        axRule exp = case exp of
            VarCons _ _ vType -> vType
            FcSymbol _ fcType -> fcType

    -- appRule t1 t2 = case t1 of

