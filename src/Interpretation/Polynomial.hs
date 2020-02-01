module Interpretation.Polynomial where
    import qualified Type.SType as Type
    import qualified Term.ATerm as Term

    {-|
        Higher-Order Polynomials over the set of a variable X.
        Each variable has a type.

        Polynomials ARE NOT FUNCTIONS (as in normal Polynomial Algebra) they are just abstractions and formal expressions that in this implementation will be built over N.
    -}

    -- | Variables has a name and a type
    data Var = Var String Type.Type


