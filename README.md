## Overview
`ski-comb` is a compiler from lambda expression to SKI-combinator.

## How to use
This code works only on `ghci`. (No main modules are in this code)<br>
You can run this by executing `ghci SKICC.hs`.
Here is some example:
```
$ ghci SKICC.hs
GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
[1 of 3] Compiling SKI              ( SKI.hs, interpreted )
[2 of 3] Compiling Common           ( Common.hs, interpreted )
[3 of 3] Compiling SKICC            ( SKICC.hs, interpreted )
Ok, modules loaded: SKICC, Common, SKI.
*SKICC> compile (LAbst "x" $ LName "x")
Loading package transformers-0.3.0.0 ... linking ... done.
Loading package mtl-2.1.2 ... linking ... done.
Loading package array-0.4.0.1 ... linking ... done.
Loading package deepseq-1.3.0.1 ... linking ... done.
Loading package containers-0.5.0.0 ... linking ... done.
I
*SKICC> compile (LAbst "x" $ LAbst "y" $ LName "x")
(S :*: (K :*: ((S :*: (K :*: K)) :*: I))) :*: I
*SKICC> evalFully $ compile (LAbst "x" $ LAbst "y" $ LName "x")
K
*SKICC> evalFully $ compile (LAbst "x" $ LAbst "y" $ LName "y")
K :*: I
*SKICC>
```
