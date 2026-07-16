#!/bin/sh

. ./env.sh

# init
mkdir -p gen/${APP_PKG}/$APP_NAME
mkdir -p bin

BGL=${BIGLOO:-$HOME/prgm/project/bigloo/5.0a/bin/bigloo}
BFLAGS=-g
BGL_ZIP=$HOME/prgm/project/bigloo/5.0a/lib/bigloo/5.0a/bigloo_s.zip
BGL_JAR=bigloo.jar
JGL=$HOME/prgm/project/bigloo/5.0a/bin/jigloo
BJF=$HOME/prgm/project/bigloo/5.0a/bin/bgljfile

# pre check
checkinst.sh || exit 1

if [ ! -f debug.keystore ]; then
   echo "*** ERROR: debug.keystore is missing"
   echo ""
   echo "Copy one from another project, or generate one with (untested):"
   echo ""
   echo "  keytool -genkeypair -alias myalias -keyalg RSA -keysize 2048 -validity 10000 -keystore debug.keystore"
   exit 1
fi

if [ " $APP_NAME" = " " ]; then
  echo "*** ERROR: no APP_NAME defined"
  exit 1
fi

# res/values/config.xml
cat > res/values/config.xml << EOF
<resources>
  <string name="app_name">$APP_NAME</string>
  <string name="app_version">$APP_VERSION</string>
  <string name="app_commit">$APP_COMMIT</string>
</resources>
EOF

# R.java
echo -n "generating gen/${APP_PKG}/$APP_NAME/R.java..."
$ANDROID_BUILD_TOOLS/aapt package -f -m -J gen -M AndroidManifest.xml -S res -I $ANDROID_PLATFORM_JAR

if [ $? != "0" -o ! -f gen/${APP_PKG}/$APP_NAME/R.java ]; then
  echo ""
  echo "\e[1;$33mfail\e[0m"
  exit 1
else
  echo "\e[1;$32mok\e[0m"
fi  

# cleaning classes
/bin/rm -f obj/${APP_PKG}/$APP_NAME/*.class

# bigloo .classes
echo -n "compiling {src,gen}/${APP_PKG}/$APP_NAME/*.scm..."
cp $BGL_ZIP $BGL_JAR

javac -Xlint:-options -source 8 -target 8 -d obj -bootclasspath $ANDROID_PLATFORM_JAR -classpath .:obj:$BGL_JAR gen/${APP_PKG}/$APP_NAME/*.java

$JGL -cp obj:$ANDROID_PLATFORM_JAR --module5 -s ${APP_PKG_CLASS}.${APP_NAME}.R -o gen/${APP_PKG}/${APP_NAME}/R.bgh
$JGL -cp obj:$ANDROID_PLATFORM_JAR --module5 -s android.util.Log -o gen/android/util/Log.bgh
$JGL -cp obj:$ANDROID_PLATFORM_JAR --module5 -s android.content.Context -o gen/android/content/Context.bgh
$JGL -cp obj:$ANDROID_PLATFORM_JAR --module5 -s android.app.Activity -o gen/android/app/Activity.bgh

echo "$BJF --strip 1 src/org/photography/LongExposure/*.bgl > .jfile"
$BJF --strip 1 src/org/photography/LongExposure/*.bgl > .jfile

echo "$BGL $BFLAGS -jvm src/org/photography/LongExposure/main.bgl -c -o obj/${APP_PKG}/${APP_NAME}/main.class"
CLASSPATH=obj:$ANDROID_PLATFORM_JAR $BGL $BFLAGS -jvm src/org/photography/LongExposure/main.bgl -c -o obj/${APP_PKG}/${APP_NAME}/main.class || exit 1
echo "$BGL $BFLAGS -jvm src/org/photography/LongExposure/log.bgl -c -o obj/${APP_PKG}/${APP_NAME}/log.class"
CLASSPATH=obj:$ANDROID_PLATFORM_JAR $BGL $BFLAGS -jvm src/org/photography/LongExposure/log.bgl -c -o obj/${APP_PKG}/${APP_NAME}/log.class || exit 1

if [ $? != "0" ]; then
  echo ""
  echo "\e[1;$33mfail\e[0m"
  exit 1
else
  echo "\e[1;$32mok\e[0m"
fi  

#  java .classes
echo -n "compiling {src,gen}/${APP_PKG}/$APP_NAME/*.java..."

javac -Xlint:-options -source 8 -target 8 -d obj -bootclasspath $ANDROID_PLATFORM_JAR -classpath .:obj:$BGL_JAR src/${APP_PKG}/$APP_NAME/*.java gen/${APP_PKG}/$APP_NAME/*.java

if [ $? != "0" ]; then
  echo ""
  echo "\e[1;$33mfail\e[0m"
  exit 1
else
  echo "\e[1;$32mok\e[0m"
fi  

# bin/classes.dex
echo -n "generating bin/classes.dex file..."
echo "$ANDROID_CMDLINE_TOOLS/d8 --min-api 21 --output bin obj/${APP_PKG}/$APP_NAME/*.class $ANDROID_PLATFORM_JAR $BGL_JAR"
$ANDROID_CMDLINE_TOOLS/d8 --min-api 21 --output bin obj/${APP_PKG}/$APP_NAME/*.class $ANDROID_PLATFORM_JAR $BGL_JAR

if [ $? != "0" ]; then
  echo ""
  echo "\e[1;$33mfail\e[0m"
  exit 1
else
  echo "\e[1;$32mok\e[0m"
fi  
  
# _unsigned.apk
echo -n "generating ${APP_NAME}_unsigned.apk..." 
$ANDROID_BUILD_TOOLS/aapt package -f -M AndroidManifest.xml -S res -I $ANDROID_PLATFORM_JAR -F ${APP_NAME}_unsigned.apk bin && (cd bin; zip -u ../${APP_NAME}_unsigned.apk classes.dex)

if [ $? != "0" ]; then
  echo ""
  echo "\e[1;$33mfail\e[0m"
  exit 1
else
  echo "\e[1;$32mok\e[0m"
fi  
  
# zip alignment
echo -n "generating $APP_NAME_aligned.apk..."
$ANDROID_BUILD_TOOLS/zipalign -f 4 ${APP_NAME}_unsigned.apk ${APP_NAME}_aligned.apk

if [ $? != "0" ]; then
  echo ""
  echo "\e[1;$33mfail\e[0m"
  exit 1
else
  echo "\e[1;$32mok\e[0m"
fi  
  
# signing
echo -n "signing ${APP_NAME}_aligned.apk..."
$ANDROID_BUILD_TOOLS/zipalign -f 4 ${APP_NAME}_unsigned.apk ${APP_NAME}_aligned.apk
echo $SIG_PASSWORD | $ANDROID_BUILD_TOOLS/apksigner sign --min-sdk-version 23 --ks debug.keystore ${APP_NAME}_aligned.apk

if [ $? != "0" ]; then
  echo ""
  echo "\e[1;$33mfail\e[0m"
  exit 1
else
  echo "\e[1;$32mok\e[0m"
fi  
  
# final copy
echo -n "generating ${APP_NAME}.apk..."

cp ${APP_NAME}_aligned.apk $APP_NAME.apk

if [ $? != "0" -o ! -f $APP_NAME.apk ]; then
  echo ""
  echo "\e[1;$33mfail\e[0m"
  exit 1
else
  echo "\e[1;$32mok\e[0m"
fi  

# optional installation

if [ " $AUTO_INSTALL" = " true" ]; then
  echo -n "installing $APP_NAME.apk on $DEVICE..."

  ./spawn.sh || exit 1
else
  echo ""
  echo "install and start with:"
  echo ""
  echo "adb -s $DEVICE install -r $APP_NAME.apk"
  echo "adb -s $DEVICE shell am start -n ${APP_PKG_CLASS}.$APP_NAME/.MainActivity"
  echo ""
  echo "AUTO_INSTALL=true"
  echo "DEVICE=serial-number"
  echo ""
  echo "to start and run automatically the application when compiled."
  echo ""
  echo "to uninstall: adb uninstall ${APP_PKG_CLASS}.$APP_NAME"
fi
