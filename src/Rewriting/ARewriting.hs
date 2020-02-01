module Rewriting.ARewriting where
    import qualified Type.SType as Type
    import qualified Term.ATerm as Term
    import qualified Data.Set as SET

    type Rule = (Term.Term, Term.Term)

    isValidRule :: Rule -> Bool
    isValidRule (l,r) = SET.isSubsetOf (Term.getVars r) (Term.getVars l)
