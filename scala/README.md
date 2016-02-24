## Overview

This is a compiler from lambda terms to SKI combinator terms, written in Scala.

## How To Use

One can test the compiler by
``` 
scalac *.scala;
java SKICompilerTest
```.

Parsing lambda terms are not supported yet.

## Dependencies
The compiler in Scala depends on scalac (at least ver. 2.11.7).
Furthermore, one needs JVM to run it.
