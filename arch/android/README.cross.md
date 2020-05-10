Android Cross Compilation
-------------------------

_9 May 2020_

This note describes how to cross compile and install Bigloo on a
Android platform, in orde to compile *native* applications on that
platform. The main objective of this native port is to let other
applications or frameworks built on top of Bigloo, for instance Hop,
to be compiled for Android.

Bigloo is **fully** operational on Android platforms but each platforms
(cpu and abi) must be built separately.

In all this document we refer to the "host", as the machine used to
compile Bigloo (typically a laptop or a desktop, running an x86 or
x86/64 architecture), and we refer to the "guest" as the android platform
that is the target of the cross compilation.

In this document, we use the Qemu emulator for cross compilation.
The three main steps of the cross compilation procedure are:

  1. getting a toolchain that is used for the low level cross compilation.
  2. preparing the device.
  3. cross compiling Bigloo.
  

### Prerequisite

In this section we will refer to `ANDROIDROOT` as the directory where we will
install all the tools and libraries needed for Android.


A. Install `adb`

On Debian this can be done with:

```shell[:@shell-host]
(in host) sudo apt install adb
```

B. Download

Create the `ANDROIDROOT` directory and download the essential components
from Google.

```shell[:@shell-host]
(in host) ANDROIDROOT=YOUR-DICTORY
(in host) mkdir -p $ANDROIDROOT/download
```

Download the ndk `.zip` file from 
[ndk](https://developer.android.com/ndk/downloads). Note that for this
you will need to accept the Google agreement.

Unzip it in `ANDROIDROOT`

```shell[:@shell-host]
(in host) cd $ANDROIDROOT
(in host) unzip download/android-ndk-r21b-linux-x86_64.zip
```

Download the `commandlinetools` from
[https://developer.android.com/studio](https://developer.android.com/studio)


Now you need to download the sdk. 

```shell[:@shell-host]
(in host) export ANDROID_HOME=$ANDROIDROOT/android-sdk-linux
(in host) yes | tools/bin/sdkmanager --sdk_root=$ANDROID_HOME  "platform-tools" "platforms;android-29"
```

The `ANDROIDROOT` at this stage should look like:

```shell[:@shell-host]
(in host) ls 
android-ndk-r21b/  android-sdk-linux/  download/  licenses/  platforms/  platform-tools/  tools/
```

C. Create a custom cc script

Create a script that will be used to invoke the C compiler for
compiling and linking Bigloo and its library:

We assume that the host platform is a linux x86/64. If using different
configuration, adjust the script below:


```shell[:@shell-host]
(in host) echo $ANDROIDROOT/cc &lt;&lt;EOF
#!/bin/bash

android=$ANDROIDROOT/android-ndk-r21b/toolchains/llvm/prebuilt/linux-x86_64
exec $android/bin/clang -target armv7a-linux-androideabi26 "$@"
EOF
```

D. Bigloo host installation

Install Bigloo for your host, as any regular Bigloo version. In the rest
of this document, we will assume that Bigloo has been installed in the
`/usr/local` directory.


### Configuring Bigloo

```[:@shell-host]
(in host) export ANDROIDPREFIX=/data/data/fr.inria.hop/assets
(in host) export ANDROIDPREFIX=/data/data/fr.inria.hop/assets
(in host) export BGLPREFIX=/usr/local
(n host) ./configure --os-android \
  --android-adb=adb \
  --cc=$ANDROIDROOT/cc \
  --cpicflags=-pic \
  --cflags="-target armv7a-linux-androideabi26 -pic -fPIC -DBGL_GC_ROOTS" \
  --lflags=-fPIC \
  --libuvconfigureopt="--host=arm-linux-androideabi" \
  --stack-check=no \
  --prefix=$ANDROIDPREFIX \
  --build-bindir=$BGLPREFIX/bin \
  --disable-doc --disable-pcre --disable-unistring --disable-gmp
```

Google provides informations about NDK and about compiling C
files. The first one to check is the [other systems][other-systems]
document that explains how to invoke the C compiler manually to
compile and link C files. A more thorough documentation can be found
in [build system][build-system] go into more details on how to compile
and link. It might also help to check the [ndk-build][ndk-build] documentation as
using `ndk-build V=1` shows the commands used to compile and link C
files.


[other-systems]: https://developer.android.com/ndk/guides/other_build_systems
[build-system]: https://android.googlesource.com/platform/ndk/+/master/docs/BuildSystemMaintainers.md
[ndk-build]: https://developer.android.com/ndk/guides/ndk-build
