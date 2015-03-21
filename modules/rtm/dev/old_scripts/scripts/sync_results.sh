#!/bin/bash

## Synchronize (rsync) results between cluster and Dropbox run_results folder.

rsync -r --progress ashiklom@geo.bu.edu:ashiklom-dietzelab/pecan/modules/rtm/run_results/FFT_Jan27/ $HOME/Documents/Dropbox/run_results/FFT_Jan27
echo Done!
