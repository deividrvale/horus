module AST.Expr where

data Exp e y = Nil |
    Const e |
    Binder e e |
    App e e

test = Const 1
