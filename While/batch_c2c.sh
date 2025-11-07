#!/bin/bash

# $1 = directory containing C files

for file in $1/*.c
  do
     d=`dirname $file`
     f=`basename $file`
     echo "Translating" $f
     for opt1 in "-b" "-s"
       do
         ./c2c.sh $file
       done
  done

