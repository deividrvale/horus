-- |
--    Module      :  $Header$
--    Description :  Provide an implementation for AFS terms.
--    Copyright   :  (c) Deivid Vale
--    License     :  MIT
--
--    Maintainer  :  deividrodriguesvale@gmail.com
--    Stability   :  experimental
--    Portability :  portable
--
--
module Term.AFS (
    -- * Types
    FSymbol,
    Term,

    -- * Types abstraction.
    Term.AFS.var,
    Term.AFS.app,
    symbol,
    Term.AFS.const,
    Term.AFS.abs,
    fApp,

    -- * Basic operation on terms
    freeNames,
    bindingVar,

    -- * Utilities

    -- * Development Only Exposure
  )
where
import qualified Type.SType as T
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

import Control.Monad.Except

{--------------------------------------------------------------------
  Datatypes
--------------------------------------------------------------------}

data Name = Name String Int
  deriving (Eq, Ord)

instance Show Name where
  show (Name x n) = x ++ show n

data FSymbol = FSymbol String [T.Type] T.Type
  deriving (Eq, Ord)

instance Show FSymbol where
  show (FSymbol s _ _) = s

data Term = Var Name | App Term Term | Abs Name Term | FApp FSymbol [Term]
  deriving (Eq, Ord)

{--------------------------------------------------------------------
  Datatype Instances
--------------------------------------------------------------------}

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

{--------------------------------------------------------------------
  Term Datatype Abstraction
--------------------------------------------------------------------}

-- | Create new variable.
var :: String -> Term
var x = Var (Name x 1)

-- | Create new application.
app :: Term -> Term -> Term
app = App

-- | Abstract a variable over a term.
abs :: Term -> Term -> Term
abs x t = case x of
  Var name -> Abs name t
  _ -> error "Fatal Error: Only variables can be abstracted. \n Please check the first argument of abs."

-- | Create a functional term.
fApp :: FSymbol -> [Term] -> Term
fApp = FApp

-- | Create a symbol signature.
symbol :: String -> [T.Type] -> T.Type -> FSymbol
symbol = FSymbol

const :: String -> T.Type -> Term
const name tp = fApp (FSymbol name [] tp) []

{--------------------------------------------------------------------
  Basic Operations on Terms
--------------------------------------------------------------------}

-- | Return the set of free names in a term.
freeNames :: Term -> (Set.Set Name)
freeNames t = case t of
    Var n -> Set.singleton n
    App m n -> Set.union (freeNames m) (freeNames n)
    Abs n m -> Set.difference (freeNames m) (Set.singleton n)
    FApp _ args -> foldl Set.union Set.empty (map freeNames args)

-- | Return the set of binding names in a term.
bindingVar :: Term -> (Set.Set Term)
bindingVar t = case t of
    Var {} -> Set.empty
    App m n -> Set.union (bindingVar m) (bindingVar n)
    Abs n m -> Set.union (Set.singleton (Var n)) (bindingVar m)
    FApp _ args -> foldl Set.union Set.empty (map bindingVar args)

-- | Return the set of indices ocurring in a term.
index :: Term -> IntSet.IntSet
index t = case t of
    Var (Name _ i) -> IntSet.singleton i
    App m n -> IntSet.union (index m) (index n)
    Abs (Name _ i) m -> IntSet.union (IntSet.singleton i) (index m)
    FApp _ [] -> IntSet.singleton 0
    FApp _ args -> foldl IntSet.union (IntSet.singleton 0) (map index args)

-- | Generate a fresh name index.
shift :: Term -> Int
shift t = (IntSet.findMax $ index t) + 1

-- | Rename a free name x of t to a fresh name y.
rename :: Term -> Name -> Name -> Term
rename t x y = if Set.member x (freeNames t) then
        case t of
            Var z -> if x == z then Var y else Var z
            App m n -> App (rename m x y) (rename n x y)
            Abs w m -> if w == x then Abs w m else Abs w (rename m x y)
            FApp f args -> FApp f (map (\t -> rename t x y) args)
    else
        t

-- -- | Rename a free variable x of t to a fresh var y not occurring anywhere in t.
-- varRename :: Term -> Term -> Term
-- varRename t v = case v of
--     Var name@(Name x _) -> rename t name (Name x (shift t))
--     otherwise -> error "Fatal Error: Only variables can be renamed in a term. Please check the second argument of 'varRename'."

-- -- | Rename all binding occurences of a variable in a term to a fresh one.
-- bindingRename :: Term -> Term -> Term
-- bindingRename t (Var v@(Name w i)) = let j = shift t in
--     recRename t (Name w j)
--     where recRename term freshName =
--             case term of
--                 Var _ -> term
--                 App m n -> App (recRename m freshName) (recRename n freshName)
--                 Abs z m -> if z == v then
--                     Abs freshName (recRename (rename m z freshName) freshName)
--                     else
--                         Abs z (recRename m freshName)
--                 FApp f args -> FApp f (map (\t -> recRename t freshName) args)
-- bindingRename _ _ = error "Fatal Error: Only variables can be renamed in a term. Please check the second argument of 'bindingRename'."

-- -- | Rename all binding names of a term that also occurs in the context domain.
-- ctxRenaming :: Context Term -> Term -> Term
-- ctxRenaming ctx t = let bs = Set.toList (ST.domain ctx `Set.intersection` bindingVar t) in
--     rename t bs
--     where rename term bindingList =
--             case bindingList of
--                 [] -> term
--                 x : xs -> rename (bindingRename term x) xs

-- | The substitution of a variable x for a term s in t.
-- Performance Note: Substitutions replace free variables only, testing this condition right away is less expensive.
-- sub :: Term -> (Name, Term) -> Term
-- sub t sub'@(x, s) = if Set.member x  (freeNames t) then
--         case t of
--             Var name -> if name == x then s else t
--             App m n -> App (sub m sub') (sub n sub')
--             Abs w@(Name y i) m -> if Set.member w (freeNames s) then
--                 Abs w' (sub m' sub') else Abs w (sub m sub')
--                     where w' = Name y (shift t)
--                           m' = rename m w w'
--             FApp f args -> FApp f (map (\t -> sub t sub') args)
--     else t

-- appSub :: Term -> (Term, Term) -> Term
-- appSub t (Var x, s) = sub t (x, s)

instance T.Typable Term where
    invRules exp = case exp of
        v@(Var {}) -> do
            tp <- T.liftLookupEnv v
            return (tp, [])

        -- Constant case.
        FApp (FSymbol _ [] tpC) [] -> do
            return (tpC, [])

        App m n -> do
            (tpM, eqM) <- T.invRules m
            (tpN, eqN) <- T.invRules n
            freshType <- T.fresh
            return (freshType, eqM ++ eqN ++ [(tpM, tpN `T.arrT` freshType)])

        Abs x m -> do
            freshType <- T.fresh
            (tpM, eqM) <- T.liftLWeaken ((Var x), freshType) (T.invRules m)
            return (freshType `T.arrT` tpM, eqM)

        FApp (FSymbol name sig tp) args -> do
            if length sig == length args
                then do
                    list <- genSigConstraints (args, sig)
                    return (tp, list)
                else throwError $ T.FunctionAppMismatch name

-- | Aux. Function to invRules: Generate the list of equational constraints based
-- on the function's signature.
genSigConstraints :: ([Term], [T.Type]) -> T.Infer Term [T.TypeEq]
genSigConstraints ([], []) = pure []
genSigConstraints x = unfold x
    where unfold (u : us, sig : sigs) = do
            (tp, eq) <- T.invRules u
            list <- (++) <$> (pure ([(tp,sig)] ++ eq)) <*> genSigConstraints (us, sigs)
            return list
