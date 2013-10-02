#!/bin/bash
for lati in {1..94} 
  do
    qsub -j y -cwd ./ncep.sh $lati $loni
done
