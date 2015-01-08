#!/bin/bash

## Synchronize (rsync) results between cluster and Dropbox run_results folder.

rsync -r --progress ashiklom@geo.bu.edu:ashiklom-dietzelab/prospect_bayes/run_results/ $HOME/Documents/Dropbox/run_results/
echo Done!
