#!/bin/bash
touch run_results/testfolder/empty
rm run_results/testfolder/*
Rscript scripts/runpi.R 0.05 Oats 1 leaf random 100 test1 testfolder
