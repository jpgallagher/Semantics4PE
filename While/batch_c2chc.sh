#!/bin/bash

# $1 = directory containing C files

for file in $1/*.c
  do
     d=`dirname $file`
     f=`basename $file`
     echo "Translating" $f
     
     # Small step semantics
     ./c2chc.sh "-s" "-t" "none" $d/$f
     for opt2 in "star" "t0" "t1" "t2"
     do 
          ./c2chc.sh "-s" "-t" "$opt2" $d/$f
     done
     
     # Big step semantics
     ./c2chc.sh "-b" "-t" "none" $d/$f
     for opt2 in "star" "t0" "t1" "t2"
     do 
          ./c2chc.sh "-b" "-t" "$opt2" $d/$f
     done
  done

