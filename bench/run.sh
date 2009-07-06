#!/bin/sh
#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bench/run.sh                         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Sep 19 13:34:36 1997                          */
#*    Last change :  Mon Aug 30 07:40:53 1999 (serrano)                */
#*    -------------------------------------------------------------    */
#*    We run the benchmarks. For each executable and we select         */
#*    the best run of several consecutive execution and we also        */
#*    print the average execution                                      */
#*=====================================================================*/
 
#*---------------------------------------------------------------------*/
#*    flags                                                            */
#*---------------------------------------------------------------------*/
cmd=""

time=/usr/bin/time 
maxrun=3

verbose=0

#*---------------------------------------------------------------------*/
#*    We parse the arguments                                           */
#*---------------------------------------------------------------------*/
while : ; do
  case $1 in
    "")
      break;;

    --bench=*|-bench=*)
      bench="`echo $1 | sed 's/^[-a-z]*=//'`";;

    --run=*|-run=*)
      maxrun="`echo $1 | sed 's/^[-a-z]*=//'`";;

    --time=*|-time=*)
      time="`echo $1 | sed 's/^[-a-z]*=//'`";;

    -v)
      verbose=1;;

    -*)
      echo "Unknown option \"$1\", ignored" >&2;;

    *)
      cmd=$1;;
  esac
  shift
done

if [ "$verbose" = "1" ]; then
  echo "[$cmd]"
fi

allt="0.0"
min="9999.99"
run=$maxrun

while [ $run -ge "1" ]; do
  
     sec=`$time -f "real: %e  sys: %S  user: %U" $cmd 2>&1 > /dev/null | sed 's/^real*//' | awk 'BEGIN {FS = " "} { sum = $4 + $6 } END { print sum }'`
                                
     if [ "$verbose" = "1" ]; then
        echo "  run $run: $sec s"
     fi

     allt=`echo "$allt + $sec" | bc -l`
     newmin=`echo "if( $sec < $min ) $sec" | bc -l`
     if [ "$newmin " != " " ]; then
        min=$newmin 
     fi

     run=`expr $run - 1`
  done

average=`echo "$allt / $maxrun" | bc -l`

if [ "$verbose" = "1" ]; then
  echo "$cmd"
  echo "  total time  : $allt s"
  echo "  average time: $average s"
  echo "  min time    : $min s"        
fi

\rm -f /tmp/$bench-res
echo "  min: $min -- average: $average -- $cmd"



  
