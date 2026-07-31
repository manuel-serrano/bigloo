#!/bin/sh
#*=====================================================================*/
#*    serrano/prgm/project/bigloo/5.0.x/bde/bglperf/bglperf.sh         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Dec  2 07:51:22 2020                          */
#*    Last change :  Fri Jul 31 08:18:11 2026 (serrano)                */
#*    Copyright   :  2020-26 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Linux Perf wrapper                                               */
#*    -------------------------------------------------------------    */
#*    For insight and details about perf, check                        */
#*      http://www.brendangregg.com/FlameGraphs/cpuflamegraphs.html    */
#*=====================================================================*/

output=
format=text
demangle=bgldemangle

exec=
args=

#*---------------------------------------------------------------------*/
#*    command line parsing                                             */
#*---------------------------------------------------------------------*/
while : ; do
  case $1 in
    "")
      break;;

    -o)
      shift;
      output=$1
      ;;
      
    --text)
      format=text
      ;;
    
    --flame)
      format=flame
      ;;

    --graph)
      format=graph
      ;;

    --no-demangle)
      demangle=cat
      ;;

    --demangle)
      shift
      demangle=$1
      ;;

    -h|--help)
      echo "Usage: bglperf [--text|--graph|--flame] [-o output] [--no-demangle] binary a0 a1 ..." >&2;
      exit 1;
      ;;
      
    *)
      if [ "$exec " = " " ]; then
        exec=$1
      else
	args="$args $1";
      fi
  esac
  shift
done

#*---------------------------------------------------------------------*/
#*    text profiling                                                   */
#*---------------------------------------------------------------------*/
if [ "$format" = "text" ]; then

  echo "generating text profile: \"$exec $args\" => $output"
  perf record -F 99 -- $exec $args
  
  if [ "$output " = " " ]; then
    perf report -n --stdio | $demangle
  else
    perf report -n --stdio | $demangle > $output
  fi
fi

#*---------------------------------------------------------------------*/
#*    text graphg profiling                                            */
#*---------------------------------------------------------------------*/
if [ "$format" = "graph" ]; then
  echo "generating text profile: \"$exec $args\" => $output"
  
  perf record -F 99 --call-graph dwarf -- $exec $args
  
  if [ "$output " = " " ]; then
    perf report -n --stdio | $demangle
  else
    perf report -n --stdio | $demangle > $output
  fi
fi

#*---------------------------------------------------------------------*/
#*    flame profiling                                                  */
#*---------------------------------------------------------------------*/
if [ "$format" = "flame" ]; then
  path=$(realpath $0)
  dir=$(dirname $path)

  if [ "$output " = " " ]; then
    output=$exec.flame.svg
  fi
  
  echo "generating flame profile: \"$exec $args\" => $output"
  perf record -F 99  --call-graph dwarf -- $exec $args
  
  perf script | $dir/stackcollapse-perf.pl --all | $demangle | $dir/flamegraph.pl --colors bigloo > $output
fi


