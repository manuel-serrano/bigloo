Bigloo Installation Notice
--------------------------

Here is the procedure for compiling and installing BIGLOO on a Unix system.

Postscript and HTML version of the Bigloo documentation can be found at:
[http://www.inria.fr/indes/fp/Bigloo](http://www.inria.fr/indes/fp/Bigloo)


### Requirements

   - For all back-ends, Bigloo installation _requires_ GNU-make. 

   - For the JVM back-end, JDK 1.5 or more recent *and* the zip
   and jar tools are required.

   - It is recommended to enable a large stack size in order to compile
   Bigloo. Usually this can be done by a command such as:
   
     $ unlimit

   - The files `fthread/README` and `fthread/INSTALL` detail the Bigloo
   threads support. 

   - The Bee emacs environment requires GNU-Emacs 21 or Xemacs 21.

   - Some Bigloo APIs require extra system libraries. For instance, the 
     SSL support requires a system development kit for SSL (the name of
     the package containing such development kit varies from one platform
     to another. For the sake of the example, under Linux Ubuntu, it is
     named `libssl-dev`).

   
### Summary

    $ ./configure
    $ make
    $ make test
    $ make compile-bee
    $ make install
    $ make install-bee
    $ make fulltest


On an Xeon E5-1650 running Linux 5.3, this whole procedure lasts about 
2 to 3 minutes.

Here are detailed each of these steps.


### Configuring Bigloo


  -  Edit the `./configure` file and set `bindir`, `libdir`, `mandir`,
  `docdir` and `cc` variables (_none_ of these variables can be 
  changed once the installation process is initiated). Alternatively,
  instead of setting these `configure` variables, you may use the
  `--prefix` option.
     
  - Configure Bigloo for your machine by invoking:

    ./configure`
	
  - By default, the Bigloo installation process install a partial version
  of the development environment BEE. In order to enable a full support
  which contains the debugger and the profiler, it is needed to configure 
  Bigloo with the following option:
  
    ./configure <your-other-options> --bee=full

  - Extra APIs can be enable or disabled at configuration time. When such an
  API is enabled, an associated Bigloo library will be compiled and 
  installed at the same location as the main Bigloo library.


### Compiling Bigloo

Type:

    make

In addition to the compiler, several tools are also compiled during
that step:

  - `BMAKE`: the Bigloo Makefile files generator.
  - `BDEPEND`: the Bigloo Makefile file dependences generator.
  - `AFILE`: the Bigloo Bigloo Module Association files generator.
  - `BPP`: the Bigloo pretty printer.
  - `BTAGS`: the Bigloo Emacs tags files generator.
  - `BPROF`: the Bigloo profiler.
  - `BMACS`: the Bigloo Emacs environment.


### Testing Bigloo

Now Bigloo is ready to be used but if you want to make the
initial test, type:

    make test

The compilation will produce _warning_ messages on several files. 
The JVM execute will produce _error_ messages. This is normal 
(the recette tests the capacity of the compiler to emit
warning messages on suspicious expressions).

This entry will test *all* configured back-ends.

On MacOSX not all tests can be executed before Bigloo is fully
install. Once installed, the extra tests can be executed with:

    make c-api-test
	  

### Compiling Bee

Bee is the Bigloo Integrated Environment. To compile it:

    make compile-bee

This compilation will produce several warnings that can be safely 
ignored.

The BEE contains the following tools:

  - `CIGLOO`: the Bigloo C file headers extractor.
  - `JIGLOO`: the Bigloo Jvm class file headers extractor.
  - `BGLMEM`: the Bigloo memory profiler.

In addition, the BEE compiles the standard Bigloo librarie in two
additional modes: debugging and profiling. 

The compilation of some files (e.g. `cigloo/Parser/parser.scm` and 
`cigloo/Parser/cpp.scm`) will produces warning message. Ignore these
messages and rest in peace.

__Note__: the emacs package requires GNU-EMACS21 or XEMACS20.4 or 
 more recent. If you don't have that version of GNU-EMACS or XEMACS 
 installed, install it or you won't benefit from the full BEE power. 
 GNU-EMACS can be downloaded from: 
   [http://www.emacs.org/](http://www.emacs.org/)
 XEMACS can be downloaded from: 
   [http://www.xemacs.org/](http://www.xemacs.org/)


### Installing 

Type

    make install

__Note__: on some architecture you will be needing to tell the 
loader where to find the Bigloo shared library. This can be done 
two ways:

  - setting the shell `LD\_LIBRARY\_PATH` variable. 
  Note: on some systems the path for dynamic libraries is specified
  by another variable. In particular, on Mac OS X, the variable is
  `DYLD\_LIBRARY\_PATH`. For instance, using the default settings 
  one should use:
  
    export LD_LIBRARY_PATH=/usr/local/lib/bigloo/<VERSION>:$LD_LIBRARY_PATH

  - updating the `/etc/ld.so.conf` file (read by ldconfig man page).

This is _not_ required on Linux nor Digital Unix but it is _required_
on SunOs and Solaris.


### Installing the Bee

   - To install the BEE, type:
   
    make install-bee

   - Edit your .emacs file to add:

    (if (locate-library "bmacs") (require 'bmacs))

   __Note__ Xemacs21 requires the SUMO distribution otherwise important 
   packages are missing. If you are still missing the sound file 
   bass-snap.au, you can pick up the version in 
   `bigloo/bmacs/etc/bass-snap.au`.


   - You can customize which modes Bee control. The default is
   Scheme, Lisp and C mode. The last two can be disabled using:
   `ESC-x: customize-group bmacs` or customizations are available through
   `ESC-x: customize-group bee` and `ESC-x: customize-group ude`.


### Testing Cigloo

   - To test Cigloo, go in the directory cigloo/Example and just type:
    make; ./ctest

   - Cigloo can be tested on a larger application. Go in the directory
   xlib` and read the `README` file.


   - Once Cigloo is tested, return to the top level directory `cd ../..`


### Uninstalling Bigloo

In order to uninstall Bigloo and the Bee, type:
  `make uninstall`


### Cleaning Bigloo

Once, installed, you can type:
    make clean
to remove all the useless files.


### Unconfiguring Bigloo

If you plan to re-install Bigloo on a new platform. Before performing
the all installation process (step 1 to 8) you must first remove the
current configuration. For this type:

    make distclean


