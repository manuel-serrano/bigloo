#! /bin/bash

if [ "$(basename $0)" == "build-android.sh" ]; then
set -e
fi

# root for all things android
ANDROIDROOT=$HOME/src/works/inria/android
# ANDROIDROOT=/misc/virtual/android

export ANDSRC=$ANDROIDROOT/eclair-git
export ANDSDK=$ANDROIDROOT/android-sdk-linux

# we can't fire a emulator automatically, so just do it yourself by hand
export ANDROID_SERIAL="emulator-5554"

# droid-wrapper
# http://github.com/tmurakam/droid-wrapper/
export DROID_ROOT=$ANDSRC
# 3 for cupcake
# 5 for eclair
export DROID_TARGET=5

# bigloo/gc
export CC=$ANDROIDROOT/droid-wrapper/bin/droid-gcc

if [ "$1" == "configure" ]; then
   ./configure \
      --prefix=$HOME/local/soft/$(basename $(pwd))-android \
      --stack-check=no \
      --disable-srfi27 \
      --gccustomversion=gc-7.2alpha4 \
      --build-bindir=$HOME/local/bin \
      --hostsh=arch/android/android-target.sh
   shift
      # --build-bindir=$HOME/local/soft/bigloo3.3b/bin \
fi

if [ "$1" == "build" ]; then
   nice -n 19 make
   shift
fi
