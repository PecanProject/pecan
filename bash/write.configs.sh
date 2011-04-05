#!/bin/bash
echo "start writing configs"
env PECANSETTINGS=$1 R --vanilla < $PECANHOME/rscripts/write.configs.R
## next: pecan.sendjobs.sh
