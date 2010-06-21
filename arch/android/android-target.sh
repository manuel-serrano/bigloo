#! /bin/sh

# this script assumes the following envvars:
# ANDROID_SDK= path to android sdk
# ANDROID_SERIAL= serial of an already runing android emulator -- not anymore, uses the first device found

binary_local_path="$1"
# initially both are the same
binary_remote_path="$1"
basename="$(basename $binary_local_path)"
adb="$ANDSDK/tools/adb"

# uname does not exist in the emulator! -- and in the phone you don't have permissions
case "$basename" in
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
   # we cannot run script in the emulator. f.i., there's no sed or test
   "config.guess")
      echo "arm-linux-gnulibc"
      ;;
   *)
      # if is not a simple command, copy over the emulator
      if [ -f "$binary_local_path" ]; then
         binary_remote_path="/data/tmp/$basename"
         # echo "pushing $binary_local_path-> $binary_remote_path" >&2
         $adb push "$binary_local_path" "$binary_remote_path"
         $adb shell chmod 755 "$binary_remote_path"
      fi

      # remove the executable from the list of parameters
      shift
      # execute and exit with its exit status
      # echo "exec'ing $binary_remote_path" >&2
      $adb shell "$binary_remote_path" $* | sed -e 's/\r//g'

      if [ -f "$binary_local_path" ]; then
         $adb shell rm $binary_remote_path
      fi
      ;;
esac
