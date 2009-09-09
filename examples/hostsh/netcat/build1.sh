#!/bin/sh
F=$1
shift
SIZE=`stat -c "%s" $F`
echo $SIZE
cat $F
echo $*
