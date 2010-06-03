#! /bin/bash

if [ "$(basename $0)" == "build-android.sh" ]; then
   set -e
fi

arch_dir=$(dirname $0)
conf_file=$arch_dir/config.sh

# import settings
if [ -f $conf_file ]; then
   source $conf_file
else
   echo "config file '$conf_file' not found, bailing out."
   exit 1
fi

if ! [ -d "$ANDROIDROOT" -a -d "$BS_BIGLOO" ]; then
   echo "config seems wrong. check config file $conf_file"
   exit 1
fi

export ANDSRC=$ANDROIDROOT/eclair-git
export ANDSDK=$ANDROIDROOT/android-sdk-linux

# droid-wrapper
# http://github.com/tmurakam/droid-wrapper/
export DROID_ROOT=$ANDSRC
# 3 for cupcake
# 5 for eclair
export DROID_TARGET=5

# gcc
export CC=$ANDROIDROOT/droid-wrapper/bin/droid-gcc

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$BS_BIGLOO/lib

if [ "$1" == "configure" ]; then
   ./configure \
      --prefix=$INSTALL_PREFIX \
      --stack-check=no \
      --disable-srfi27 \
      --gccustomversion=gc-7.2alpha4 \
      --build-bindir=$BS_BIGLOO/bin \
      --hostsh=arch/android/android-target.sh
   shift
fi

if [ "$1" == "build" ]; then
   nice -n 19 make
   shift
fi
