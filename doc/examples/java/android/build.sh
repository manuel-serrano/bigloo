#!/bin/sh

. ./env.sh

# init
mkdir -p bin
mkdir -p obj/${APP_PKG}/${APP_NAME}

# debug.keystore
if [ ! -f debug.keystore ]; then
  keytool -genkeypair -v -keystore debug.keystore -storepass android -alias androiddebugkey -keypass android -keyalg RSA -keysize 2048 -validity 10000 -dname "CN=Android Debug,O=Android,C=US"
fi

# bigloo .classes
cp $BIGLOO_ZIP bigloo.jar

# R.java/R.class
$ANDROID_BUILD_TOOLS/aapt package -f -m -J src -M AndroidManifest.xml -S res -I $ANDROID_PLATFORM_JAR

javac -Xlint:-options -source 8 -target 8 -d obj -bootclasspath $ANDROID_PLATFORM_JAR -classpath .:obj:bigloo.jar src/${APP_PKG}/${APP_NAME}/{R,AndroidUtils}.java

# R.bgh, AndroidUtils.bgh
$JIGLOO -cp obj:$ANDROID_PLATFORM_JAR --module5 -s ${APP_PKG_CLASS}.${APP_NAME}.R -o src/${APP_PKG}/${APP_NAME}/R.bgh
$JIGLOO -cp obj:bigloo.jar:$ANDROID_PLATFORM_JAR --module5 -s ${APP_PKG_CLASS}.${APP_NAME}.AndroidUtils -o src/${APP_PKG}/${APP_NAME}/AndroidUtils.bgh

# .jfile
$JFILE --strip 1 src/${APP_PKG}/${APP_NAME}/*.bgl > .jfile

# main.class
CLASSPATH=obj:$ANDROID_PLATFORM_JAR $BIGLOO $BFLAGS -jvm src/${APP_PKG}/${APP_NAME}/main.bgl -c -o obj/${APP_PKG}/${APP_NAME}/main.class || exit 1

javac -Xlint:-options -source 8 -target 8 -d obj -bootclasspath $ANDROID_PLATFORM_JAR -classpath .:obj:bigloo.jar src/${APP_PKG}/${APP_NAME}/*.java

# bin/classes.dex
$ANDROID_CMDLINE_TOOLS/d8 --min-api 21 --output bin obj/${APP_PKG}/${APP_NAME}/*.class $ANDROID_PLATFORM_JAR bigloo.jar

# _unsigned.apk
$ANDROID_BUILD_TOOLS/aapt package -f -M AndroidManifest.xml -S res -I $ANDROID_PLATFORM_JAR -F ${APP_NAME}_unsigned.apk bin && (cd bin; zip -u ../${APP_NAME}_unsigned.apk classes.dex)

# zip alignment
$ANDROID_BUILD_TOOLS/zipalign -f 4 ${APP_NAME}_unsigned.apk ${APP_NAME}_aligned.apk

# signing
$ANDROID_BUILD_TOOLS/zipalign -f 4 ${APP_NAME}_unsigned.apk ${APP_NAME}_aligned.apk
echo $SIG_PASSWORD | $ANDROID_BUILD_TOOLS/apksigner sign --min-sdk-version 23 --ks debug.keystore ${APP_NAME}_aligned.apk

# final copy
cp ${APP_NAME}_aligned.apk ${APP_NAME}.apk
