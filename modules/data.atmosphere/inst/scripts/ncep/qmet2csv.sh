#!/bin/bash
for lati in {1..94} 
  do
  for loni in {93..192}
    do
    qsub -j y -cwd ./met2csv.sh $lati $loni
  done
done
