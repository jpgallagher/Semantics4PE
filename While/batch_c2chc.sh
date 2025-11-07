#!/bin/bash

# $1 = directory containing C files

for file in $1/*.c
  do
     d=`dirname $file`
     f=`basename $file`
     echo "Translating" $f
     for opt1 in "-b" "-s"
       do
         for opt2 in "none" "star" "t0" "t1" "t2"
           do 
             ./c2chc.sh $opt1 "-t" "$opt2" $d/$f
           done
       done
  done

