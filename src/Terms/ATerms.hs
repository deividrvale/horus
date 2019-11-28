module Terms.ATerms
    (
      Term
    ) where

    import qualified Type.SType as TYPE

    -- data VName = VName String Int

    data Var = VCons String Int TYPE.Type

    data FSymbol = FArity String TYPE.Type

    data Term = Var | App FSymbol [Term]



