#!/bin/bash
echo "start writing configs"
env PECANSETTINGS=$1 R --vanilla < ./rscripts/write.configs.R

