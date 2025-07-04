#!/bin/sh

AST="/Users/jpg/recsolv/bigstep/CParser"
WHILE="."

# Interpret the script options as options for "while"

prg=$1
f=`basename $prg`

f=${f%.c} # remove .c extension

d=`dirname "$prg"`
resultdir="$d"/"$f"_chc

if (test ! -d $resultdir) then
        mkdir $resultdir
fi


# C parsing
java -jar "$AST"/xcfp.jar "$prg" > "$resultdir"/parse.out
$AST/ast4sem "$resultdir"/parse.out "$resultdir"/ast.out

# Transform disjunctive loops
$WHILE/c2c "$resultdir"/ast.out -o "$resultdir"/"$f"_trans.c
