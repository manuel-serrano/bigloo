<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/string.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Main page                                                     -->
<!--==================================================================-->

Bigloo (aka bgl), is an optionally typed dynamic language. It supports:

  * 3 backends: native, jvm, and wasm;
  * AoT compilation via an optimizing compiler;
  * modules for separate compilation;
  * preemptive multi-threading with shared memory;
  * an object system based on generic function;
  * exception handling;
  * rich set of builtin libraries;
  * safe and unsafe modes;
  
Bigloo's design found its inspiration in several other programming languages:

  * Lisp, for its parenthetical syntax;
  * Scheme, for its core language;
  * ML, for its pattern-matching facility;
  * JavaScript for its module system;
  * CommonLisp Object System, for its generic functions;
  * C/Posix for its multi-threading a many system operations.
  
Bigloo focuses on a seamless integration with native execution platforms. 

  * When compiling to native code, the Bigloo code can be interfaced with 
  C code. The binary produced can be executed natively on Linux and MacOS.
  * When compiling to JVM bytecode, the Bigloo code can be interfaced with
  Java code. The compiled programs can run on any JVM implementation, 
  including Android OS.
  * When compiling to WASM, the generated code can be interfaced with
  hand-written wasm code and can be executed on any Wasm platform, including
  web browsers.
