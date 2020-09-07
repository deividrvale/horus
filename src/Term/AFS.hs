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
    var,
    Term.AFS.app,
    symbol,
    Term.AFS.const,
    Term.AFS.abs,
    fApp,

    -- * Utilities
    typeChecking,
    genTypeEq,
    freeNames,
    varRename,
    appSub
  )
where
import Type.SimpleTypes as ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

{--------------------------------------------------------------------
  Datatypes
--------------------------------------------------------------------}

data Name = Name String Int
  deriving (Eq, Ord)

instance Show Name where
  show (Name x n) = x ++ show n

data FSymbol = FSymbol String [ST.Type] ST.Type
  deriving (Eq, Ord)

instance Show FSymbol where
  show (FSymbol s _ _) = s

data Term = Var Name | App Term Term | Abs Name Term | FApp FSymbol [Term]
  deriving (Eq, Ord)

type Subst = [(Name, Term)]

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
symbol :: String -> [ST.Type] -> Type -> FSymbol
symbol = FSymbol

const :: String -> ST.Type -> Term
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
bindingNames :: Term -> (Set.Set Name)
bindingNames t = case t of
    Var n -> Set.empty
    App m n -> Set.union (bindingNames m) (bindingNames n)
    Abs n m -> Set.union (Set.singleton n) (bindingNames m)
    FApp _ args -> foldl Set.union Set.empty (map bindingNames args)

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

-- | Rename a free name x of t to a fresh name y not occurring anywhere in t.
rename :: Term -> Name -> Name -> Term
rename t x y = if Set.member x (freeNames t) then
        case t of
            Var z -> if x == z then Var y else Var z
            App m n -> App (rename m x y) (rename n x y)
            Abs w m -> if w == x then Abs w m else Abs w (rename m x y)
            FApp f args -> FApp f (map (\t -> rename t x y) args)
    else
        t

-- | Rename a free variable x of t to a fresh var y not occurring anywhere in t.
varRename :: Term -> Term -> Term
varRename t v = case v of
    Var name@(Name x i) -> rename t name (Name x (shift t))
    otherwise -> error "Fatal Error: Only variables can be renamed in a term. Please check the second argument of 'varRename'."

-- | The substitution of a variable x for a term s in t.
-- Performance Note: Substitutions replace free variables only, testing this condition right away is less expensive.
sub :: Term -> (Name, Term) -> Term
sub t sub'@(x, s) = if Set.member x (freeNames t) then
        case t of
            Var name -> if name == x then s else t
            App m n -> App (sub m sub') (sub n sub')
            Abs w@(Name y i) m -> if Set.member w (freeNames s) then
                Abs w' (sub m' sub') else Abs w (sub m sub')
                    where w' = Name y (shift t)
                          m' = rename m w w'
            FApp f args -> FApp f (map (\t -> sub t sub') args)
    else t

appSub :: Term -> (Term, Term) -> Term
appSub t (Var x, s) = sub t (x, s)

{--------------------------------------------------------------------
  Implementation of Simple Types typeclass
--------------------------------------------------------------------}
instance SimpleTypedCurry Term where
    axiom ctx asgn = ST.member asgn ctx

    declareType s@(Var _) t = ST.newAssignment s t
    declareType s@(FApp _ []) t = ST.newAssignment s t
    declareType _ _ = error "Fatal Error: Type declaration are only possible for variables and constants."

    typeChecking ctx v@(Var x) tp = case ST.getType ctx v of
        Nothing -> Left False
        Just tp' -> undefined

genTypeEq :: Context Term -> Term -> Type -> Maybe [(Type, Type)]
genTypeEq ctx v@(Var x) tp = (\t -> [(tp, t)]) <$> ST.getType ctx v

genTypeEq ctx (App m n) tp = (++) <$> genTypeEq ctx m (ST.newArrowType fname tp) <*> genTypeEq ctx n fname
    where fname = ST.freshTypeVar tp

genTypeEq ctx (Abs name m) tp = (++) <$> genTypeEq ctx' m fname2 <*> pure [(ST.newArrowType fname1 fname2, tp)]
    where fname1 = ST.freshTypeVar tp
          fname2 = ST.freshTypeVar fname1
          ctx' = ST.add (ST.declareType (Var name) fname1) ctx

genTypeEq ctx (FApp (FSymbol _ sig a) args) tp = if length sig == length args
    then
        case (args, sig) of
            ([], []) -> pure [(a, tp)]
            (x : xs, t : ts) -> (++) <$> genTypeEq ctx x t <*> unfold (xs, ts)
                where unfold ([], []) = pure [(a, tp)]
                      unfold (x' : xs', t' : ts') = (++) <$> genTypeEq ctx x' t' <*> unfold (xs', ts')
    else
        Nothing
