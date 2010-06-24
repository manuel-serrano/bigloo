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

version=$(awk -F '=' '/^RELEASE/ { print $2 }' Makefile.config)

if [ "$1" == "build" ]; then
   nice -n 19 make
   # "install" the makefiles. needed by hop
   ( cd lib/$version
      ln -s ../../Makefile.config .
      ln -s ../../Makefile.misc .
   )
   shift
fi
