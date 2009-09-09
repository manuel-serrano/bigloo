#!/bin/sh
TMPFILE=`mktemp`
DIR=`dirname $0`
TARGET=$1; shift;
EXEC=$1;
if [ ! -f $EXEC ]; then
  # seems to be a unix command (most likely uname)
  echo $* > $TMPFILE;
  EXEC=$TMPFILE;
fi

PORT=12345
PORT2=12346
function delay_cat() {
  # echo "---------------- SENDING TO HOST: " $EXEC 1>&2;
  cat $EXEC;
  sleep 0.5
  # echo "---------------- SENT. waiting for response... " $EXEC 1>&2;
}
sleep 0.5;
delay_cat | netcat -c -w 1 $TARGET $PORT;
netcat -w 1 $TARGET $PORT2 | $DIR/build2.sh;
RES=$?
/bin/rm $TMPFILE
exit $RES
