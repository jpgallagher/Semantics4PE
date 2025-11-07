#!/bin/bash

# $1 = test directory
# $2 = outfile module

PE="/Users/jpg/Library/CloudStorage/OneDrive-RoskildeUniversitet/Research/LP/clptools/predabs/pe" 


# initialise output file
cfile=$1"/"$2".pl"
echo ":- module("$2",_)." >  $cfile
echo ":- multifile comparison/9." >> $cfile

for file in $1/*.c
   do
     f=`basename $file`
     f=${f%.c} # remove .c extension
     echo $f
     $PE/compareCost $1/results.pl $f >> $cfile
   done