#!/bin/bash
for i in {1..24}
do 
  ssh compute-0-$i "rm -rf /scratch/*"
done