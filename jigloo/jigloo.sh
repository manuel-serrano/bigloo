#!/bin/sh

path=`realpath $0`
dir=`dirname $path`

classpath=$dir:$PWD

if [ " $1" = " -cp" ]; then
  classpath="$classpath:$2"
  shift;
  shift
elif [ " $1" = " -classpath" ]; then
  classpath="$classpath:$2"
  shift;
  shift
fi

java -cp $classpath  jigloo $@
