#!/bin/bash

# $1 = test directory
# $2 = output module

PE="/Users/jpg/Library/CloudStorage/OneDrive-RoskildeUniversitet/Research/LP/clptools/predabs/pe" 


# initialise results file
rfile=$1"/"$2".pl"
echo ":- module("$2",_)." >   $rfile
echo ":- multifile result/6." >> $rfile


for file in $1/*.c
   do
     ./analyse.sh $file $rfile
   done