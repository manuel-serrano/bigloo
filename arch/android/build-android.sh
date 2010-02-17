#! /bin/bash

set -e

# root for all things android
ANDROIDROOT=$HOME/src/works/inria/android
# ANDROIDROOT=/misc/virtual/android

# export ANDSRC=$ANDROIDROOT/cupcake-git
export ANDSRC=$ANDROIDROOT/eclair-git
export ANDSDK=$ANDROIDROOT/android-sdk-linux

# adv="test-1.5"
# try to communicate with the emulator, or fire it if is not there.
# we can't fire one automatically, so just do it yourself by hand
export ANDROID_SERIAL="emulator-5556"

# droid-wrapper
# http://github.com/tmurakam/droid-wrapper/
export DROID_ROOT=$ANDSRC
# export DROID_TARGET=generic
# 3 for cupcake
# 5 for eclair
export DROID_TARGET=5

# bigloo/gc
export CC=$ANDROIDROOT/droid-wrapper/bin/droid-gcc
# export CFLAGS="-DPLATFORM_ANDROID" # this should not be needed
# export CFLAGS="-DPLATFORM_ANDROID -DLINUX"

if [ "$1" == "reconfigure" ]; then
   ./configure \
      --prefix=$HOME/local/soft/$(basename $(pwd)) \
      --stack-check=no \
      --hostsh=arch/android/android-target.sh --disable-srfi27 \
      --build-bindir=$HOME/local/soft/bigloo3.3b/bin \
      --gccustomversion=gc-7.2alpha4
   shift
fi

if [ "$1" == "build" ]; then
   nice -n 19 make
   shift
fi
