#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
library(testthat)
library(PEcAn.DB)


settings <- list(database = 
                   list(userid = "bety", 
                        passwd = "bety", 
                        host = "localhost",
                        name = "bety"))
if(db.exists()){
  system("echo 'testing if bety exists on forecast'|mail -s 'sent from ebi forecast' dlebauer@gmail.com")
} else {
  system("echo 'bety does not exist on forecast'|mail -s 'sent from ebi forecast' dlebauer@gmail.com")
}
test_package("PEcAn.DB")

