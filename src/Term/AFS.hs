{- |
    Module      :  $Header$
    Description :  Define simple types rules and type checking function and classes.
    Copyright   :  (c) Deivid Vale
    License     :  MIT

    Maintainer  :  deividrodriguesvale@gmail.com
    Stability   :  experimental
    Portability :  portable

    Defines simple types rules, type checking
-}
module Term.AFS (
    FSymbol,
    Term,
    var,
    app,
    symbol,
    Term.AFS.abs,
    fApp,
) where
    import qualified Type.SimpleTypes as ST

    data Name  = Name String Int
        deriving (Eq, Ord)

    instance Show Name where
        show (Name x n) = x ++ show n

    data FSymbol = FSymbol String ST.Type
        deriving Eq

    instance Show FSymbol where
        show (FSymbol s _) = s

    data Term = Var Name | App Term Term | Abs Name Term | FApp FSymbol [Term]
        deriving Eq

    instance Show Term where
        show (Var name) = show name
        show (App s t)  = "(" ++ show s ++ " " ++ show t ++ ")"
        show (Abs x t)  = "\\" ++ show x ++ "-> " ++ show t
        show (FApp f args) = printFunctionApp (FApp f args) 0

    -- Auxiliary function for show instance of Term.
    -- | Get a function application data and print its f(t_1, ..., t_n) string output form.
    printFunctionApp :: Term -> Int -> String
    printFunctionApp (FApp f args) i
        | i == 0 && null args       = show f
        | i == 0 && not (null args) = show f ++ "(" ++ printFunctionApp (FApp f args) (i + 1)
        | i > 0  = case args of
            [] -> ")"
            (ti : ts) -> if null ts then
                show ti ++ printFunctionApp (FApp f ts) (i + 1)
                                        else show ti ++ ", " ++ printFunctionApp (FApp f ts) (i + 1)
    printFunctionApp _ _ = undefined


    -- External functions for AFS Abstraction.

    var :: String -> Term
    var x = Var (Name x 1)

    app :: Term -> Term -> Term
    app = App

    abs :: Term -> Term -> Term
    abs x t = case x of
        Var name -> Abs name t
        _ -> error "Fatal Error: Only variables can be abstracted. \n Please check the first argument of abs."

    fApp :: FSymbol -> [Term] -> Term
    fApp = FApp

    symbol :: String -> ST.Type -> FSymbol
    symbol = FSymbol
