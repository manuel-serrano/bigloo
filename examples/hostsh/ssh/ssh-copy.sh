#!/bin/sh

HOST=localhost
SSH_PORT=2022
USER=hop

FILE=$1
FILEAUX=$2

if [ "$FILEAUX " != " " -a -f $FILEAUX ]; then
  exec=/tmp/`basename $FILEAUX`
  scp -P $SSH_PORT $FILEAUX $USER@$HOST:$exec
  ssh -p $SSH_PORT $USER@$HOST "chmod +x $exec"
fi

if [ -f $FILE ]; then
  exec=/tmp/`basename $FILE`
  scp -P $SSH_PORT $FILE $USER@$HOST:$exec
  ssh -p $SSH_PORT $USER@$HOST "chmod +x $exec"
else
  exec=$*
fi

ssh -p $SSH_PORT $USER@$HOST "$exec"
res=$?
ssh -p $SSH_PORT $USER@$HOST "rm $exec"
exit $res
