<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/jvm.md                   -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    JVM backend                                                   -->
<!--==================================================================-->

,(example-path "../test/src/extern_jvm.bgl")

JVM Backend
===========
<!-- [:@jvm] -->

We call all the pieces of program devoted to the interactions between
Bigloo and another language a _foreign interface_. This document
describes the Jvm interface that is available when using the C
backend. The [C](./c.html) and [wasm](./wasm.html) interfaces are
described in dedicated chapters.

The Bigloo jvm foreign interface allows import of Java classes, which
enables Bigloo code to invoke Java methods (static or not) and to
access object fields and class static fields. It also enables application
to export functions for Java code.

To generate Java class files, the Bigloo compiler has to be invoked
with the `-jvm` command line option. See `bigloo -help` for all the
Jvm related option. 

When compiling to Java, Bigloo defines the property cond-expand's `jvm` 
property.

> [!NOTE] The Jvm interface does not support Java class definitions. 
> Consequently, programming environments that requires new classes to be
> declared, need a mix of Java code and Bigloo. A complete example can
> be found in the Android section.

Introduction
------------

Connecting Bigloo code with Java is generally straightforward. To
illustrate this simplicity, let us consider a simple example involving
three source files. First a Java interface `Intf.java`:

```Java
,(include "../test/src/Intf.java" :line-start 11)
```

And a Java class implementation.

```Java
,(include "../test/src/Point.java" :line-start 11)
```

The interface and the class can be used by Bigloo  modules such as:

```bigloo
,(include "../test/src/extern_jvm.bgl" :tag "doc")
,(include "../test/src/extern_jvm.bgl" :tag "doc2")
```

Note that in this example, the Java class `Point` and the Java interface
`Intf` are _imported_ from Java and the Bigloo definition `callback`
is _exported_ to Java.

Extern "java" Module Clause
---------------------------

```bnf
<MJExtern> --> ( extern "java" <MJClause>* )

<MJClause> --> <MJPackage>
  | <MJImport>
  | <MJArray>
  | <MJExport>
  
<MJPackage> --> ( package <Ident> )
  
<MJImport> --> ( class <TypedIdent> <MJCtor>* <MJProperty>* )
  | ( abstract-class <TypeIdent> <MJMethod>* )

<MJCtor> --> ( constructor <TypedIdent> <TypedIdent>* )
  | ( constructor <TypedIdent> <TypedIdent>* <String> )

<MJProperty> --> <MJField> | <MJMethod>

<MJField> --> ( field <TypedIdent> )
  | ( static field <TypedIdent> )

<MJMethod> --> ( <MJQualifier>* <TypedIdent> <TypeIdent>* )
  | ( <MJQualifier>* <TypedIdent> <TypeIdent>* <String> )

<MJQualifier> --> abstract | static | public | final

<MJArray> --> ( array <Ident> <TypedIdent> )

<MJExport> --> ( export <Ident> )
```

Bigloo code invoke the constructors, the static methods, and the
instance methods with different syntax:

  * The Bigloo identifiers of constructors and static methods are made
  by the concatenatingf the class name and the declared
  constructor. For instance `Point.new`.  A constructor is invoked
  from Bigloo code by calling it as a regular function.
  
  * Object methods are invoked using the `((-&gt; o method) arg ...)`.
  
Java Packages
-------------

Java and the JVM strongly relates file names to class names and packages.
Java uses `qualified` class names, that is a mean to implement 
name spaces. Two classes with the same name but contained in different
packages are diffent.

Bigloo compiles module to class files and offers two means to control
are they are mappend to Java qualified class names. First, the
module clause `(package &lt;Ident&gt;)` maps the class being compile
to a qualified Java class name. For instance, when compiling a module

```bigloo
;; file ex.bgl
(module ex
  (extern "java"
     (package org.bigloo))
  ...)
```

Bigloo will use the `org.bigloo.ex` qualified name for generated class.

Second the tool `bgljfile` can maps source file names to qualified type
names. This tool generate a file named `.jfile` which is an association
map that, if exists in the directory from which Bigloo is invoked, is
read by the compiler, and is used to map file names to Java qualifed
class name. For instance, to map the `ex` module to the qualified
class name, without a `package` module clause, one may use the following
.jfile:

```
((ex "org.bigloo.ex"))
```

See `bgljfile -help` for more options.

When importing a Java class defined in a package, its fully qualified
name of that class is required. For instance, if a Bigloo modules
needs the Android Java class `Intent`, it must imported with a
declaration such as:

```bigloo
(extern "java"
   ...
   (class android.content.Intent
      (public getAction::String)
      ...)
   ...)
```

But Bigloo binds imported class names to their fully qualified type name
and relative name, which can be used
as a shorthand. To keep elaborating on the previous example, once the
`android.content.Intent` is declared, the shorter name `Intent` can
be used instead. For instance, as in:

```bigloo
(extern "java"
   ...
   (class android.content.Context
      (field final public static CAMERA_SERVICE::String)
      (field final public static LOCATION_SERVICE::String)
      (abstract public registerReceiver::Intent ::BroadcastReceiver ::IntentFilter)
      (abstract public unregisterReceiver::void ::BroadcastReceiver)
      ...)
   ...)
```

Java Classes
------------

Java Arrays
-----------

Jigloo
------

Android
-------

This section explains how to prepare a development environment for
Android with a linux setting. Then, it shows how to develop a 
Bigloo Android app and how to install it on an actual device.

This section assumes that the variable shell variable `ANDROID_HOME`
contains the name of the directory where tools are installed.

### No IDE
This section explains how to download and prepare the tools needed to
compile and install Bigloo Android apps. When the installation is complete,
applications can be developped as any other Bigloo applications, that is,
with regular tools (such as shell and emacs) and does not require any
dedicated IDE (such as Android Studio).

> [!WARNING] Android installation is a moving target because of constant
> revision changes accompanied with new tools and/or depreacted methods
> and frequent URL changes. These constant changes are
> likely to obsolete this section quickly.

> [!WARNING] The installation cannot be fully automatized because some
> steps requires accepting licenses. The Android environment installation
> is then an interactive process executed from with a shell.
  
#### Linux requirements

The firt tool to be installed is `adb` which enables communication with 
the device. On Debian this can be done with:

```shell
apt install adb
```

#### Select and create the directory

```shell
export ANDROID_HOME=/opt/android
mkdir -p $ANDROID_HOME
mkdir -p $ANDROID_HOME/home/android
mkdir -p $ANDROID_HOME/home/android/avd
mkdir -p $ANDROID_HOME/home/android/cache
```

#### Install command line tools

Download the command line tools from:

[http://developer.android.com/sdk/index.html](http://developer.android.com/sdk/index.html)

As of 206, the name of the file to be download is: 
  `commandlinetools-linux-14742923.latest.zip`

unzip it

```shell
(cd $ANDROID_HOME; unzip commandlinetools-linux-13114758_latest.zip)
```

#### Set shell variables

```
cat > env.sh <<EOF
export ANDROID_HOME=/opt/android
export ANDROID_PLATFORM=34
export ANDROID_PLATFORM_VERSION=$ANDROID_PLATFORM.0.0
export ANDROID_SDK_VERSION=$ANDROID_PLATFORM.0.0
export ANDROID_NDK_VERSION=27.3.13750724
export ANDROID_USER_HOME=$ANDROID_HOME/home/android
export ANDROID_AVD_HOME=$ANDROID_USER_HOME/avd
export ANDROID_BUILD_TOOLS=$ANDROID_HOME/build-tools/$ANDROID_PLATFORM_VERSION
export ANDROID_CMDLINE_TOOLS=$ANDROID_HOME/cmdline-tools/bin

export ANDROID_PLATFORM_JAR=$ANDROID_HOME/platforms/android-$ANDROID_PLATFORM/android.jar

export PATH=$PATH:$ANDROID_HOME/build-tools/$ANDROID_PLATFORM_VERSION
export PATH=$PATH:$ANDROID_HOME/cmdline-tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools
export PATH=$PATH:$ANDROID_HOME/emulator
EOF
chmod a+rx env.sh
```

#### Install Android tools

```shell
. env.sh
yes | sdkmanager --licenses --sdk_root=$ANDROID_HOME
sdkmanager "system-images;android-$ANDROID_PLATFORM;google_apis;x86_64" "platform-tools" "platforms;android-$ANDROID_PLATFORM" "build-tools;$ANDROID_SDK_VERSION" "ndk;$ANDROID_NDK_VERSION" --sdk_root=$ANDROID_HOME
avdmanager create avd --name Pixel_5_API_$ANDROID_PLATFORM --package "system-images;android-$ANDROID_PLATFORM;google_apis;x86_64" --device "pixel_5"
ln -s `pwd` .
(cd $HOME; ln -s $ANDROID_HOME/home/android/android .android)
```

#### To start the emulator and logs

```shell
. env.sh
emulator -avd Pixel_5_API_34
adb -s 0A091FDD4007CN logcat | grep -E "LongExposure| System.err"
```

To reset the emulated machine

```shell
emulator -avd Pixel_5_API_34 -wipe-data
```

### A Complete Example

