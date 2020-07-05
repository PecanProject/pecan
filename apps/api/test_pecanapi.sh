#!/bin/bash

R R/entrypoint.R & 
PID=$!

R test/alltests.R
kill $PID