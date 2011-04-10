#!/bin/bash
OUTDIR=$1
cd $OUTDIR
echo "zipping config files to saconfigs.tgz"
tar -zcf configs.tgz c.* ED2INc.*
rm c.* ED2INc.*
