#!/bin/sh
EXEC=`mktemp /tmp/exec_XXXXXX`
RES=`mktemp /tmp/res_XXXXXX`
#echo "---------------- Receiving file " 1>&2;
cat > $EXEC
#echo "---------------- Received. Executing" 1>&2;
chmod u+x $EXEC
eval $EXEC > $RES
STATUS=$?
echo $STATUS
#echo "---------------- Sending back." 1>&2;
cat $RES
rm $EXEC
rm $RES
