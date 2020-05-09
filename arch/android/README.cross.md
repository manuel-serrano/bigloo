Android Cross Compilation
-------------------------

_9 May 2020_

This note describes how to cross compile and install Bigloo on a
Raspberry PI. The procedure is complex because of the different ARM
platforms and because of the variety of tools that are needed for
completing this installation. 

In all these documents we refer to the "host", as the machine used to
compile Bigloo (typically a laptop or a desktop, running an x86 or
x86/64 architecture), and we refer to the "guest" as the arm platform
that is the target of the cross compilation.

In this document, we use the Qemu emulator for cross compilation.
Qemu can be replaced with an actual raspberry device.

The three main steps of the cross compilation procedure are:

  1. getting a toolchain that is used for the low level cross compilation.
  2. preparing qemu to emulate a raspberry device.
  3. cross compiling Bigloo.
  
  
### Prerequisite

