#! /bin/bash

if [ "$(basename $0)" == "build-android.sh" ]; then
   set -e
fi

arch_dir=$(dirname $0)
conf_file=$arch_dir/config.sh
hostsh=$arch_dir/android-target.sh
INSTALL_PREFIX=$(pwd)/arch/android/usr

# import settings
if [ -f $conf_file ]; then
   source $conf_file
else
   echo "config file '$conf_file' not found, bailing out."
   exit 1
fi

if [ "$1" == "configure" ]; then
   # try to detect which tmp dir to use
   if ! $hostsh noconf ls /tmp | grep 'No such' > /dev/null; then
      echo "tmp_dir=/tmp" > $arch_dir/config-phone.sh
   elif ! $hostsh noconf mkdir /sdcard/tmp | grep 'Permission denied' > /dev/null; then
      echo "tmp_dir=/sdcard/tmp" > $arch_dir/config-phone.sh
   else
      echo "Could not find a suitable tmp dir in the device." >&2
      exit 1
   fi

   ./configure \
      --prefix=$INSTALL_PREFIX \
      --stack-check=no \
      --disable-srfi27 \
      --gccustomversion=gc-7.2alpha4 \
      --build-bindir=$BS_BIGLOO/bin \
      --hostsh=$hostsh

   shift
fi

version=$(awk -F '=' '/^RELEASE/ { print $2 }' Makefile.config)

if [ "$1" == "build" ]; then
   make
   make install
   shift
fi
