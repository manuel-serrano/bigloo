#! /bin/bash

if [ "$(basename $0)" == "build-android.sh" ]; then
   set -e
fi

arch_dir=$(dirname $0)
conf_file=$arch_dir/config.sh
hostsh=$arch_dir/android-target.sh
INSTALL_PREFIX=$(pwd)/arch/android/usr

function test_tmp_dir {
   dir=$1
   ok=0

   # test the dir exists
   if $adb shell ls $dir | grep -iq 'no such'; then
      # no; try to create it
      if $adb shell mkdir $dir | egrep -iq '(permission denied|read-only)'; then
         ok=1
      # else
      #    ok=0
      fi
   # else
   #    ok=0
   fi

   if [ $ok -eq 0 ]; then
      # the directory exists
      # try to create an exec
      if $adb shell "echo 'echo yes' > $dir/foo" | grep -iq 'read-only'; then
         # read only
         ok=1
      else
         # set it executable and run it
         $adb shell chmod 755 $dir/foo
         if $adb shell $dir/foo | grep -iq 'permission denied'; then
            # can't execute
            ok=1
         fi

         # cleanup
         $adb shell rm $dir/foo
      fi
   fi

   return $ok
}

# import settings
if [ -f $conf_file ]; then
   source $conf_file
else
   echo "config file '$conf_file' not found, bailing out."
   exit 1
fi
adb="$ANDSDK/tools/adb"

if [ "$1" == "configure" ]; then
   # try to detect which tmp dir to use
   tmp_dir=""
   for dir in /tmp /sdcard/tmp /data/local/tmp; do
      if test_tmp_dir $dir; then
         tmp_dir=$dir
         echo "tmp_dir=$dir" > $arch_dir/config-phone.sh
         break
      fi
   done

   if [ -z "$dir" ]; then
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
