Bigloo, a practical Scheme compiler
===================================

[![Travis](https://api.travis-ci.org/manuel-serrano/bigloo.svg)](https://travis-ci.org/manuel-serrano/bigloo/builds)

Bigloo is a Scheme development environment that includes a compiler
generating C code and Java classes, and an interpreter. Bigloo is the
tool of choice for building autonomous applications in Scheme. It
is mostly conformant to the Revised5 Report on the Algorithmic
Language Scheme with many extensions: 
  * lexical and syntactic builtin parser generators;
  * pattern-matching;
  * foreign languages interface (connection to C and to Java);
  * modules;
  * object-oriented class-based programming;
  * preemptive multi-threading;
  * unicode characters and strings;
  * posix programming (process, pipe, socket);
  * openssl;
  * multimedia libraries;
  * event loops and event-based programming (libuv).


Versions
--------

The Bigloo git version is intended for contributors and testers. Regular
Bigloo users, _i.e.,_ programmers willing to use Bigloo and compile and
run their own applications, should use the stable pre-bundled or
pre-compiled versions available at:

   [http://www-sop.inria.fr/indes/fp/Bigloo/download.html](http://www-sop.inria.fr/indes/fp/Bigloo/download.html)


Installation
------------

In order to bootstrap the git Bigloo, you first need to install a
pre-bundled version, whose installation procedure is described
in [INSTALL.md](INSTALL.md) file. 

To bootstrap the git version, proceed as follows:

  1. Install the latest stable or unstable version. Let us assume that the 
 installation directories are `<lib-path-dir>` and `<bin-path-dir>`
  2. configure the git Bigloo version with:
 `./configure --prefix=<my-prefix>`
  3. bootstrap the compiler with: 
 `LD_LIBRARY_PATH=<lib-path-dir> make bigboot BGLBUILDBINDIR=<bin-path-dir>`
  4. install that bootstrapped version:
 `make install-progs`
  5. compile all the libraries and complete the bootstrap:
 `make fullbootstrap-sans-log`
  6. install everything:
 `make install-sans-docs`
 
See the `doc` directory for extra hints and information about the Bigloo
installation process.

 
Overview
--------

The compiler distribution consists of several directories:

  * `bin`       contains the compiler executable.
  * `lib`       contains the Bigloo's libraries.
  * `comptime`  contains the sources of the compiler.
  * `runtime`   contains the sources of the runtime system.
  * `examples`  contains various Bigloo's examples.
  * `recette`   contains compiler and runtime test programs.
  * `manuals`   contains the documentation of the Bigloo system.
  * `doc`       contains various documents related to the Bigloo installation.
  * `etc`       contains various ancillary files
  * `tools`     contains severals tools files required for the boot of Bigloo.
  * `cigloo`    contains the sources the C headers to Bigloo headers translator.
  * `jigloo`    contains the JVM classes to Bigloo headers translator.
  * `xlib`      contains an example of connecting Bigloo and the X library.
  * `contrib`   contains the list of currently available Bigloo contributions.
  * `bde`       contains the source code of the Bee (the Bigloo IDE).
  * `bmacs`     contains the emacs-lisp part of the Bee.
  * `tutorials` contains a tutorial that briefly presents the capacities of the Bee.
  * `bdl`       contains the Bigloo development library.
  * `bglpkg`    contains the Bigloo packages system.
  * `win32`     contains scripts for installing JVM Bigloo based version on win32.
  * `api`       contains various Bigloo libraries.
  * `gc`        contains the garbage collector source files and ad-hoc makefiles.
  * `libuv`     contains the libuv source files and ad-hoc makefiles.
  * `pcre`      contains the pcre source files and ad-hoc makefiles.
  * `www`       contains the Bigloo web site sources.


Acknowledgments
---------------

Many people have helped developping Bigloo, specialy Hans J. Boehm who
wrote the garbage collector, Jean-Marie Geffroy who found many bugs
and who has written `Match`, Christan Queinnec for all his valuable
comments and help, Dominique Boucher who wrote the Lalr system and
some contribs, Jay 'Eraserhead' Felice who added precedence operators
to Lalr grammars, Stephen J Bevan, Joris Heirbaut who ported Bigloo
under Solaris, Drew Whitehouse who ported Bigloo under Iris Indigo,
Luc Moreau, Pascal Petit, Joel Masset and Thierry Saura, Laurent
Bloch, Christopher Oliver who pointed me bugs out, Thomas Neumann who
ported Bigloo under NeXT, Olaf Burkart for his comments on Bigloo,
John Gerard Malecki who point me out bugs and suggested many
improvements, David Gurr who points me out bugs, Rodrigo Vanegas who
suggested some extensions, Kenneth Dwayne Ray for all its suggestions,
Michael Sperber and Vincent Kieffer for the AIX port, Dave Love which
fix many bugs, Jacques Garrigue which helped me, Marcel Turcotte,
Alexandre Frey, Raj Manandhar which points me bugs or documentation
problems, Alain Mellan that provide me with the `Format`
implementation, Dorai Sitaram for his portable implementation of Posix
regular expressions. I specially thank Barrie Stott who widely
contributes to the documentation improvements, Erick Gallesio for our
discussions and for all the code from STk that I have integrated
inside Bigloo, Frédéric Boussinot for his help collaboration to the
Bigloo thread library, Bernard Serpette who is the author of the JVM
back-end, Yannis Bres who has developped the .NET runtime no longer in
used today and who has improved the C and Java runtimes, and Sven
Hartrumpf a tireless tester and contributor. A special attention to
Vladimir Tsichevski and Yann Dirson for there unestimable help.
