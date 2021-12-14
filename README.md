# Horus

Horus is a basic Higher-Order rewriting and unification tool for complexity analysis of different Higher-Order formalisms.
It is conceived to be simple and modular, so it can be used by other projects as well.

---

## Basic Setup on Linux

You can compile the project on any system that satisfies all dependencies below:

* [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) package management,

* GHC v8.8.3 — see [the Haskell Platform installation instructions](https://www.haskell.org/platform/linux.html#linux-generic),

* System build dependency: **Z3**, installation on Ubuntu is easy

        sudo apt-get install z3 libz3-dev
