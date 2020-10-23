-- |
--    Module      :  $Header$
--    Description :  Defines the behavior of any substitution.
--    Copyright   :  (c) Deivid Vale
--    License     :  MIT
--
--    Maintainer  :  deividrodriguesvale@gmail.com
--    Stability   :  experimental
--    Portability :  portable
--
--    Defines classes for substitution used in any AST over the library.
{-# LANGUAGE MultiParamTypeClasses #-}

module AST.Subst where

{--------------------------------------------------------------------
  Classes
--------------------------------------------------------------------}

class Substitutable sub exp where
    apply :: sub -> exp -> exp
