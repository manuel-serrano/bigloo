#!/bin/sh
NETCAT=$1
PORT=12345
PORT2=12346
while [ 1 ]; do
    $NETCAT -l -p $PORT | ./host_exec.sh | $NETCAT -c -l -p $PORT2
done
