# Horus

This project is a basic implementation of notions from Higher-Order rewriting and, for now, it is  conceived to be a simple tool for the analysis of some Higher-Order Rewriting Systems.

More information will be added later.

## Note

This is a learning project. So if you are an expert developer in Haskell do expect some naive implementation. Even some non-standard development practice. I'm working hard to minimize that as I learn more Haskell in the future.

I have two main concern in the development:

* Learn Haskell programming language in a deep level. Nothing better than create a problem to solve. Right?

* The implementation of pure mathematical notions in functional language.

With all these things considered, at a first moment efficiency is not my primary concern (but I do the best I can to run things as fast as possible).

---

## Basic Setup

To compile and run the project you need:

* The [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) package management.

* GHC v8.6.5, see [the installation instructions](https://www.haskell.org/platform/linux.html#linux-generic) for more details on how to install The Haskell Platform.

For development purposes I'm also using:

* [Visual Studio Code](https://code.visualstudio.com/) with this [plugin](https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server) to support Haskell development.

* For a complete Haskell support install [Haskell IDE Engine](https://github.com/haskell/haskell-ide-engine#installation).

---

## Higher-Order Rewriting in Haskell

### Applicative Higher-Order Rewriting

<!--
```ocaml
    (* Variables Names *)
    type varName = string * int

    (* Inductive type of firs-order-terms *)
    type term =
        | Var of varName
        | F of string * int * term list
``` -->
