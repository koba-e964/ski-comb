## Overview
`ski-comb` is a set of compilers from lambda terms to SKI-combinator terms, with the algorithm introduced in [http://www.tatapa.org/~takuo/kotori_ski/](http://www.tatapa.org/~takuo/kotori_ski/).

Every *closed* lambda term can be converted to an SKI-combinator term, which consists of *combinators* `S`,`K` and `I`, and function applications. This fact corresponds to the fact that Hilbert-style deductive system is as expressive as natural deduction and intuitionistic sequent calculus.

The compilers are written in Haskell and Scala.
## The Compiler in Haskell
The compiler in Haskell depends on GHC (at least ver. 7.6.3).
For more detail, see `haskell/README.md`.

## The Compiler in Scala
The compiler in Scala depends on scalac (at least ver. 2.11.7).
Furthermore, one needs JVM to run it.

In progress...