#!/bin/sh

REMOTE_HOST=localhost
REMOTE_SSH_PORT=2022
REMOTE_USER=hop

FILE=$1;

if [ -f $FILE ]; then
  exec=/tmp/`basename $FILE`
  scp -P $REMOTE_SSH_PORT $FILE $REMOTE_USER@$REMOTE_HOST:$exec
  ssh -p $REMOTE_SSH_PORT $REMOTE_USER@$REMOTE_HOST "chmod +x $exec"
else
  exec=$*
fi
ssh -p $REMOTE_SSH_PORT $REMOTE_USER@$REMOTE_HOST "$exec"
res=$?
ssh -p $REMOTE_SSH_PORT $REMOTE_USER@$REMOTE_HOST "rm $exec"
exit $res
