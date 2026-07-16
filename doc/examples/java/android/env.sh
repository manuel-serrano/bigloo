#!/bin/sh

# android configuration
export ANDROID_HOME=/opt/android

export ANDROID_PLATFORM=34
export ANDROID_PLATFORM_VERSION=$ANDROID_PLATFORM.0.0
export ANDROID_SDK_VERSION=$ANDROID_PLATFORM.0.0
export ANDROID_NDK_VERSION_OLD_UNUSED=25.2.9519653/
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

#* export AVD=${AVD:-Pixel_5_API_34}                                   */
#* export DEVICE=${DEVICE:-emulator-5554}                              */
#* export APP_PKG=org/bigloo                                           */
#* export APP_PKG_CLASS=`echo $APP_PKG | sed 's|/|.|'`                 */
#* export SIG_PASSWORD=${SIG_PASSWORD:-android}                        */
#* export AUTO_INSTALL=${AUTO_INSTALL:-false}                          */
#*                                                                     */
#* export APP_NAME=GPS                                                 */
#* export APP_VERSION=1.0.0                                            */
#*                                                                     */
#* # bgl configuration                                                 */
#* export BGL=${BGL:-/usr/local/bin/bigloo}                            */
#* export BGL_BIN_DIR=`dirname $BGL`                                   */
#* export BGL_ZIP_DIR=`$BGL -eval "(begin (print (bigloo-config 'zip-directory)) (exit 0))"` */
#* export BGL_VERSION=`$BGL -eval "(begin (print (bigloo-config 'release-number)) (exit 0))"` */
#*                                                                     */
