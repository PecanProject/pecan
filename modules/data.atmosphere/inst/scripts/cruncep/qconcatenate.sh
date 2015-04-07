#!/bin/bash
for year in 1901 1911 1921 1931 1941 1951 1961 1971 1981 1991 2001; do
    qsub -j y -S /bin/bash ./concatenate.sh
done