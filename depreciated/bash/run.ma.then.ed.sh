#!/bin/bash
cd /home/dlebauer/pecan
./bash/query.bety.sh $1
wait
./bash/meta.analysis.sh $1
wait
./bash/write.configs.sh $1
wait
./bash/start.runs.sh $1