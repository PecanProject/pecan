#!/bin/bash
for lati in {1..94} 
  do
  for loni in {1..192}
    do
    qsub -j y -cwd ./ncep.sh $lati $loni
  done
done
