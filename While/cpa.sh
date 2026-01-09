#!/bin/bash

# $1 = file 

PE="/Users/jpg/Library/CloudStorage/OneDrive-RoskildeUniversitet/Research/LP/clptools/predabs/pe" 
CHCLIBS="/Users/jpg/Library/CloudStorage/OneDrive-RoskildeUniversitet/Research/LP/clptools/predabs/chclibs/src" 



OPTSTRING="r"

rank=""

while getopts ${OPTSTRING} flag 
do
   case "${flag}" in  
       r) rank="-rank"; 
	   ;;
   esac
done

shift $(( OPTIND - 1 ))

file=$1

echo "% Analysing" $file
d=`dirname $file`
f=`basename $file`
f=${f%.pl} # remove .pl extension

echo $f

wp=widenpoints

options="-narrowiterations 1 -delaywidening 0 -withwut -threshold"

$PE/drawcfg -prg $file -o "$d"/cfg.txt
dot -Tjpg -o "$d"/cfg.jpg "$d"/cfg.txt

chclibs-thresholds1 -prg $file -a -o $d/wut.props

$CHCLIBS/cpascc_new -prg $file $options $d/wut.props "-widenpoints" $d/$wp $rank -o $d/$f.cha.pl
$PE/observeCpasccAns -prg $d/$f.cha.pl -f $f

