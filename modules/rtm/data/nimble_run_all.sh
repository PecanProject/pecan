#! /bin/bash
## Read in SE species and pass to 'species_run.sh'
##
## Arguments to species_run.sh:
## [1] Spectra
## [2] Number of iterations
## [3] Folder

NG=5e5
FD="FFT_individuals_0203"

while read l; do
        echo $l
        nimble_run.sh $l $NG $FD
done < FFT_fullspecnames.txt
