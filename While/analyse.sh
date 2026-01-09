#!/bin/bash

# $1 = file name
# $2 = results file

PE="/Users/jpg/Library/CloudStorage/OneDrive-RoskildeUniversitet/Research/LP/clptools/predabs/pe" 

     file=$1
     d=`dirname $file`
     f=`basename $file`
     f=${f%.c} # remove .c extension
     chcdir="$d"/"$f"_chc
     # initialise results file
     # results="$d"/"$f"_results.pl
     
     results="$2" 
     
     if ! test -f $results; then
      echo "" > $results
     fi


	 echo "% Analysing" $f.c
     echo "% Analysing" $f.c >> $results
     echo "% =================" >> $results
     # 
     for i in "1" "2" "3" "4" "0"
        do
        for narrow in  "0" "1" "2"
          do 
          for suffix in "none" "star_rank" "t0_rank" "t1_rank" "t2_rank"
            do
            suffix1="_big_right_""$suffix"
        	chcversion="$f""$suffix1".pl
            specoutput="$d"/"$f"_chc/"$f""$suffix1".pl_output/"$f""$suffix1".qa.cha.pl
            #echo $chcversion >> $results 
            $PE/spec.sh "$chcdir"/"$chcversion" "go__0" "$i" "$narrow"
            $PE/observeAns -prg "$specoutput" -f "$f" -t "$i" -n "$narrow" -s "$suffix" >> $results 
          done
       done
    done
