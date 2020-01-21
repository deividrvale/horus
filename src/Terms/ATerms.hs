module Terms.ATerms where
    import qualified Type.SType as TYPE

    data Term = VarCons String Int TYPE.Type | FcSymbol String TYPE.Type | AppCons Term Term TYPE.Type
        deriving (Eq, Show)

    -- | Instantiate a new typed variable with the initial index.
    newVar :: String -> TYPE.Type -> Term
    newVar name = VarCons name 0

    -- | Rename all variables in a term by increasing it's index.
    renameVar :: Term -> Term
    renameVar term = case term of
        VarCons name i t -> VarCons name (i + 1) t
        FcSymbol name t -> FcSymbol name t
        AppCons s1 s2 t -> AppCons (renameVar s1) (renameVar s2) t

    -- | The creation of full applied functions from a function symbol.

    buildApp :: Term -> Term

    buildApp (FcSymbol name t) = case t of
        (TYPE.Base _) -> FcSymbol name t
        (TYPE.Arrow t1 t2) -> buildApp (AppCons (FcSymbol name t) (newVar "x" t1) t2)

    buildApp (AppCons s t tp) = case tp of
        (TYPE.Base n) -> (AppCons s t tp)
        (TYPE.Arrow t1 t2) -> buildApp (AppCons (AppCons s t tp) (newVar "y" t1) t2)







    -- Implementation of TypeCheck Class
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

