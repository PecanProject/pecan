#!/bin/bash
echo "begin extracting results from model output"
env PECANSETTINGS=$1 R --vanilla <rscripts/get.model.output.R
echo "end extracting results from model output"