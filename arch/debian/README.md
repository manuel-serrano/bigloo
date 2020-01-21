Bigloo Debian Package 19 jan 2020
=================================

This document explains how to use the `makedeb.sh' script to
build the Bigloo Debian packages.


To build the debian packages on the local machine
-------------------------------------------------
  
  ./makedeb.sh [-O targetdir] [--repodir dir]
  
example:

  ./makedeb.sh -O /tmp/debbigloo


To build the debian packages on a remote machine via ssh
--------------------------------------------------------
  
  ./makedebremote.sh mach
