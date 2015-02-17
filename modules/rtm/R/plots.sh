#! /bin/bash
while read l; do
        echo $l
        qsub -q "geo*" ab.sh $l
done < FFTspecies_plots.txt


