## Overview

This is a compiler from lambda terms to SKI combinator terms, written in Scala.

## How To Use

One can test the compiler by the command below.
``` 
make compile
java Main 1plus1.txt 1plus1.out
```

This command compiles the file `1plus1.txt` and outputs the resulting SKI term to `1plus1.out`.

```
$ cat 1plus1.txt
(\add. \one. add one one) ((\succ. \n. \m. n succ m) (\n. \f. \x. f (n f x))) (\f. f)
$ cat 1plus1.out
S S (K I) (S (K (S I)) K (S (K (S (S (S (K S) (S (K K) (S (K S) K))) (S (K (S I)) K)))) K)) I
```

## Dependencies
The compiler in Scala depends on scalac (at least ver. 2.11.7).
Furthermore, one needs JVM to run it.
