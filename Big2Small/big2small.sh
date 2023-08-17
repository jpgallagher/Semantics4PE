#!/bin/sh

BIG2SMALL="."

logengoal="run(_)"


f=`basename $1`
f=${f%.pl} # remove .pl extension

d=`dirname "$1"`

resultdir="$d"/"$f"_big2small


if (test ! -d $resultdir) then
        mkdir $resultdir
fi

$BIG2SMALL/normalise "$1" "$resultdir"/"$f".norm.pl
$BIG2SMALL/clp2logen "$resultdir"/"$f".norm.pl "$resultdir"/"$f".logen

# build logen annotated file
cat $BIG2SMALL/smallStepSolve.pl.ann "$resultdir"/"$f".logen > $BIG2SMALL/smallStepSolveWithprogram.pl.ann

# logen
cogen -np $BIG2SMALL/smallStepSolveWithprogram.pl -m $logengoal > "$resultdir"/$f.pe


rm -f *.itf *.po *.gx *.cpx $BIG2SMALL/smallStepSolveWithprogram.pl.ann "$resultdir"/"$f".logen
#rm -rf "$resultdir"



