-- |
--    Module      :  $Header$
--    Description :  Define simple types rules and type checking function and classes.
--    Copyright   :  (c) Deivid Vale
--    License     :  MIT
--
--    Maintainer  :  deividrodriguesvale@gmail.com
--    Stability   :  experimental
--    Portability :  portable
--
--    Defines simple types rules, type checking.
module Term.AFS
  ( FSymbol,
    Term,
    var,
    Term.AFS.app,
    symbol,
    Term.AFS.const,
    Term.AFS.abs,
    fApp,
    typeChecking,
    genTypeEq
  )
where

import Type.SimpleTypes as ST

data Name = Name String Int
  deriving (Eq, Ord)

instance Show Name where
  show (Name x n) = x ++ show n

data FSymbol = FSymbol String ST.Type
  deriving (Eq, Ord)

instance Show FSymbol where
  show (FSymbol s _) = s

data Term = Var Name | App Term Term | Abs Name Term | FApp FSymbol [Term]
  deriving (Eq, Ord)

instance Show Term where
  show (Var name) = show name
  show (App s t) = "(" ++ show s ++ " " ++ show t ++ ")"
  show (Abs x t) = "\\" ++ show x ++ "-> " ++ show t
  show (FApp f args) = printFunctionApp (FApp f args) 0

-- Auxiliary function for show instance of Term.

-- | Get a function application data and print its f(t_1, ..., t_n) string output form.
printFunctionApp :: Term -> Int -> String
printFunctionApp (FApp f args) i
  | i == 0 && null args = show f
  | i == 0 && not (null args) = show f ++ "(" ++ printFunctionApp (FApp f args) (i + 1)
  | i > 0 = case args of
    [] -> ")"
    (ti : ts) ->
      if null ts
        then show ti ++ printFunctionApp (FApp f ts) (i + 1)
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

const :: FSymbol -> Term
const c = fApp c []

-- Implementation of Simple Types type class
instance SimpleTypedCurry Term where
    axiom ctx asgn = ST.member asgn ctx

    declareType s@(Var _) t = ST.newAssignment s t
    declareType s@(FApp _ []) t = ST.newAssignment s t
    declareType _ _ = error "Fatal Error: Type declaration are only possible for variables and constants."

    typeChecking ctx v@(Var x) tp = case ST.getType ctx v of
        Nothing -> Left False
        Just tp' -> undefined

genTypeEq :: Context Term -> Term -> Type -> Maybe ([(Type, Type)])
genTypeEq ctx v@(Var x) tp = (\t -> [(tp, t)]) <$> ST.getType ctx v

genTypeEq ctx (App m n) tp = (++) <$> genTypeEq ctx m (ST.newArrowType fname tp) <*> genTypeEq ctx n fname
    where fname = ST.freshTypeVar tp

genTypeEq ctx (Abs name m) tp = (++) <$> genTypeEq ctx' m fname2 <*> pure [(ST.newArrowType fname1 fname2, tp)]
    where fname1 = ST.freshTypeVar tp
          fname2 = ST.freshTypeVar fname1
          ctx' = ST.add (ST.declareType (Var name) fname1) ctx
