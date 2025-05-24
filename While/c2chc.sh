#!/bin/sh

# Examples
# ./c2chc.sh -s /Users/jpg/Desktop/test2.c 
# ./c2chc.sh -b /Users/jpg/Desktop/test2.c 
# ./c2chc.sh -b -r -t /Users/jpg/Desktop/test2.c 
# ./c2chc.sh -b -l /Users/jpg/Desktop/test2.c 

AST="/Users/jpg/recsolv/bigstep/CParser"
WHILE="."

# Interpret the script options as options for "while"

yn="no"
style="small"
recursion="right"

OPTSTRING="bslrt"

while getopts ${OPTSTRING} flag 
do
   case "${flag}" in  
       b) style="big"; 
	   ;;
       s) style="small"; 
	   ;;
	   t) yn="yes";
	   ;;
	   r) recursion="right"; 
	   ;;
	   l) recursion="left"; 
	   ;;
   esac
done

shift $(( OPTIND - 1 ))

prg=$1
f=`basename $prg`
echo "$f"
f=${f%.c} # remove .c extension

d=`dirname "$prg"`
resultdir="$d"/"$f"_chc

if (test ! -d $resultdir) then
        mkdir $resultdir
fi


# C parsing
java -jar "$AST"/xcfp.jar "$prg" > "$resultdir"/parse.out
$AST/ast4sem "$resultdir"/parse.out "$resultdir"/ast.out

# PE
logengoal="go("\'"$resultdir"/ast.out\'",$style"",$recursion"",$yn"")"
# echo "$logengoal"

# logen
cogen -np $WHILE/"while.pl" -m "$logengoal" > "$resultdir"/$f.pe

# unfold determinate calls

case "${yn}" in  
       yes) trans="trans"; 
	   ;;
       no) trans="notrans"; 
	   ;;
esac

outname=$f"_"$style"_"$recursion"_"$trans".pl"

# echo $outname

chclibs-unfoldForward -prg "$resultdir"/$f.pe -entry "go__0" -det -eq -o "$resultdir"/$outname
