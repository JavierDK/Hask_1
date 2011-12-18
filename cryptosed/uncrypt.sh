#! /bin/bash

pwd=""

function gen {
  if [ $1 -eq 4 ]
  then
    s=""
    for i in $(seq 0 3)
    do
     s="$s${a[$i]}" 
    done
    rm ./tex2html-test.tar
    if 7z e -p$s ./tex2html-test.tar.7z
    then
      pwd=$s
    fi
    echo $s
  else
    for j in $(seq 0 9)
    do
      a[$1]=$j
      gen $[$1 + 1] 
    done
  fi
}

gen 0

rm ./tex2html-test.tar

7z e -p$pwd ./tex2html-test.tar.7z
7z e ./tex2html-test.tar
