#!/bin/bash
R CMD INSTALL ..
R -d gdb -e "source('fortinv.test.R')"
