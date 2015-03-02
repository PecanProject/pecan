#! /bin/bash
## Read in SE species and pass to 'species_run.sh'
## 
## Arguments to species_run.sh:
## [1] Species (spectype for this script)
## [2] Random effects ("none", "leaf")
## [3] Initial conditions ("random", "mle", "guess")
## [4] Number of iterations 
## [5] Results folder

NG=$1
FD=$2

while read l; do
        echo $l
        nimble_run.sh $l $NG $FD
done < ../R/FFT_fullspecnames.txt
