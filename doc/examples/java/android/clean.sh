#!/bin/sh

. ./env.sh

/bin/rm -f debug.keystore
/bin/rm -f src/${APP_PKG}/${APP_NAME}/R.java
/bin/rm -f src/${APP_PKG}/${APP_NAME}/R.bgh
/bin/rm -f bigloo.jar
/bin/rm -rf .jfile

/bin/rm -f cnt_unsigned.apk cnt_aligned.apk cnt_aligned.apk.idsig

/bin/rm -rf obj
/bin/rm -rf bin

find . -name 'flycheck_*' -exec /bin/rm {} \;
