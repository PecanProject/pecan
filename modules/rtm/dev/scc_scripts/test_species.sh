#! /bin/bash
## Read in SE species and pass to 'species_run.sh'
## 
## Arguments to species_run.sh:
## [1] Species (spectype for this script)
## [2] Random effects ("none", "leaf")
## [3] Initial conditions ("random", "mle", "guess")
## [4] Number of iterations 
## [5] Results folder

SPEC=$1
RE=$2
IC=$3
NG=$4
FD=$5

while read l; do
        echo $l
        species_run.sh $l $RE $IC $NG $FD
done <species_list_"$SPEC".txt
