<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/c.md                     -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Demangling                                                    -->
<!--==================================================================-->

Demangling
==========

When generated C or jvm byte code, Bigloo mangles identifiers. The
Bigloo functions for mangling and demangling identifiers are described
in the [Bigloo Specifics](./bigloo.html) chapter. For convenience,
they are packaged into a binary tool that could be used in conjunction
with Unix OS tools such as `ldd`, `perf` or `ident` to display
information using the true source identifiers.

The tools `bgldemangle` reads from the standard input mangled
identifiers and write to the standard output the corresponding
dmeangled ones. It leaves unmangled identifiers unchanged.

For instance, let us consider the following module

```bigloo
;; fib.bgl
module fib
   (export fib))

(define (fib::long x::long)
   (if (<fx x 2)
       1
       (+fx (fib (-fx x 1)) (fib (-fx x 2)))))
```

Let us compile it with

```shell
$ bigloo -unsafe fib.bgl -rm
```

The `-rm` option tells Bigloo not to delete the generated C file.
Then

```shell
$ cat fib.c | indent | bgldemangle | tail -n 29 | head -n 14
   BGL_EXPORTED_DEF long fib@fib(long x)
   {
      {	/* fib.bgl 4 */
	 if ((x < 2L))
	   {	/* fib.bgl 5 */
	      return 1L;
	   }
	 else
	   {	/* fib.bgl 5 */
	      return (fib@fib((x - 1L)) + fib@fib((x - 2L)));
	   }
      }

   }
```

