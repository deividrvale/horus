module Term.ATerm (
    Term(..),
    SubMap,
    renameVar,
    getVars,
    createSignature,
    buildApp,
    appSub,
    isSubterm
    ) where
    import qualified Type.SType as TYPE
    import qualified Data.Set as SET
    import qualified Type.SimpleTypes as ST

    data Term = VarCons String Int TYPE.Type | FcSymbol String TYPE.Type | AppCons Term Term TYPE.Type
        deriving (Eq, Show, Ord)

    type SubMap = [(Term, Term)]
    type Signature = SET.Set Term

    -- | Rename all variables in a term by increasing it's index.
    renameVar :: Term -> Term
    renameVar term = case term of
        VarCons name i t -> VarCons name (i + 1) t
        FcSymbol name t -> FcSymbol name t
        AppCons s1 s2 t -> AppCons (renameVar s1) (renameVar s2) t

    appSub :: Term -> SubMap -> Term
    appSub term [] = term
    appSub term ( (dom,img) : tl) = case term of
        VarCons {} -> if dom == term then img else appSub term tl
        FcSymbol {} -> term
        AppCons t1 t2 tp -> AppCons (appSub t1 ((dom,img) : tl) ) (appSub t2 ((dom,img) : tl) ) tp

    -- | Helper Function. Returns the set of variable occuring in a term.
    getVars' :: Term -> SET.Set Term -> SET.Set Term
    getVars' (VarCons name index tp) vars = SET.insert (VarCons name index tp) vars
    getVars' (FcSymbol _ _) vars = SET.empty
    getVars' (AppCons t1 t2 _) vars = SET.union (getVars' t1 vars) (getVars' t2 vars)

    -- | Return the set of variables ocurring in a term. Its always starts with a empty set.
    getVars :: Term -> SET.Set Term
    getVars t = getVars' t SET.empty

    {-|
        The subterm relation, i.e., t is a subterm of s written as s >= t iff
            s = t or s > t where s1 s2 > t if
                s1 > t or s2 >= t
        This function is just the above definition defined recursively.
    -}
    isSubterm :: Term -> Term -> Bool
    isSubterm s t = s == t || proper s t where
        proper s t = case s of
            VarCons {} -> False -- Variables don't have proper subterms.
            (FcSymbol _ _) -> False -- Function Symbols don't have proper subterms.
            (AppCons s1 s2 _) -> proper s1 t || isSubterm s2 t

    -- getSubTerms' :: Term -> SET.Set Term -> SET.Set Term
    -- getSubTerms' t set = case t of
    --     (VarCons x y z) -> SET.insert (VarCons x y z) set
    --     FcSymbol {} -> SET.empty
    --     AppCons t1 t2 _ -> if (isSubterm t t1) && (isSubterm t t2) then
    --         SET.union (getSubTerms' t1 (SET.insert t1 set)) (getSubTerms' t2 (SET.insert t2 set))
    --         else
    --             SET.empty

    -- | Build a signature from a list of terms ignoring other terms that is not a function symbol.
    createSignature' :: [Term] -> SET.Set Term -> SET.Set Term
    createSignature' [] set = set
    createSignature' (hd : tl) set = case hd of
        FcSymbol name tp -> createSignature' tl (SET.insert (FcSymbol name tp) set)
        _ -> createSignature' tl set

    createSignature :: [Term] -> SET.Set Term
    createSignature lst = (createSignature' lst SET.empty)

    -- | Helper Function. Returns a fully applied term from a function symbol. Each argument is a variable X_i.
    buildApp' :: Term -> Int -> Term
    buildApp' (FcSymbol name tp) i = case tp of

        (TYPE.Base _) -> FcSymbol name tp
        (TYPE.Arrow t1 t2) -> buildApp' (AppCons (FcSymbol name tp) (VarCons "x" (i + 1) t1) t2) (i + 1)
    buildApp' (AppCons s t tp) i = case tp of
        (TYPE.Base _) -> AppCons s t tp
        (TYPE.Arrow t1 t2) -> buildApp' (AppCons (AppCons s t tp) (VarCons "x" (i + 1) t1) t2) (i + 1)

    -- | Returns a fully applied term from a function symbol. Each argument is a variable X_i.
    buildApp :: Term -> Term
    buildApp t = buildApp' t 0

    -- | Implementation of TypeCheck Class
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
