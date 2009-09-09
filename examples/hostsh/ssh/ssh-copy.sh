#!/bin/sh
TARGET=$1; shift;
FILE=$1;
if [ -f $FILE ]; then
  EXEC=/tmp/`basename $FILE`
  scp $FILE $TARGET:$EXEC
  ssh $TARGET "chmod +x $EXEC"
else
  EXEC=$*
fi
ssh $TARGET "$EXEC"
RES=$?
ssh $TARGET "rm $EXEC"
exit $RES
