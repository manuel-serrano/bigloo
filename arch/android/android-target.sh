#! /bin/sh

# this script assumes the following envvars:
# ANDROID_SDK= path to android sdk
# ANDROID_SERIAL= serial of an already runing android emulator -- not anymore, uses the first device found

# set -e

# load the tmp_dir from the phone config
if [ "$1 " = "noconf " ]; then # == is a bashism!
   shift
else
   . $(dirname $0)/config-phone.sh # source is anothe bashism
fi

binary_local_path="$1"
# initially both are the same
binary_remote_path="$1"
basename="$(basename $binary_local_path)"
adb="$ANDSDK/tools/adb"

case "$basename" in
   # uname does not exist in the emulator! -- and in the phone you don't have permissions
   "uname")
      case "$2" in
         -m)
            echo "arm";;
         -s)
            echo "android";;
         *)
            echo "";;
      esac
      ;;
   # we cannot run the script in the emulator. f.i., there's no sed or test
   "config.guess")
      echo "arm-linux-gnulibc" # yes, I know, is not glibc...
      ;;
   *)
      # if is not a simple command, copy over the emulator
      if [ -f "$binary_local_path" ]; then
         binary_remote_path="$tmp_dir/$basename"
         $adb push "$binary_local_path" "$binary_remote_path" > /dev/null
         $adb shell chmod 755 "$binary_remote_path" > /dev/null
      fi

      # remove the executable from the list of parameters
      shift
      # execute and exit with its exit status
      $adb shell "$binary_remote_path" $* | sed -e 's/\r//g'

      if [ -f "$binary_local_path" ]; then
         $adb shell rm $binary_remote_path > /dev/null
      fi
      ;;
esac
