Bigloo Debian Package - 19 jan 2020
===================================

This document explains how to use the `makedeb.sh' script to
build the Bigloo Debian packages.

This script install the Bigloo version out of a tarball stored in:

   $HOME/prgm/distrib

It uses the currently "configured" Bigloo version.


1. Prerequisite
---------------

Install the Debian development toolchain and the Bigloo dependencies
on the host. In particular

  sudo apt -qq update
  sudo apt install -y dh-make libssl1.1 libssl-dev libsqlite3-0 libsqlite3-dev
  sudo apt install -y libasound2-dev libflac-dev libmpg123-dev libavahi-core-dev
  sudo apt install -y libavahi-common-dev libavahi-client-dev libpulse-dev
  sudo apt install -y libgmp-dev automake libtool


2. To build the debian packages on the local machine
----------------------------------------------------
  
  ./makedeb.sh [-O targetdir] [--repodir dir]
  
example:

  ./makedeb.sh -O /tmp/debbigloo


3. To build the debian packages on a remote machine via ssh
-----------------------------------------------------------
  
  ./makedebremote.sh [-O targetdir] [host] [user]
  
Warning! This assumes that bash is available on the remote host

example:

  ./makedebremote.sh -O /tmp/debraspbian raspbian hop


4. Building Debian packages with Qemu
-------------------------------------

The file bigloo/arch/raspian/README.cross.md contains informations on
how to prepare an ARM image suitable for the Qemu emulator and how to
spawn the emulator so that it can be used to build a Debian package
on the emulated architecture.
