
# these packages are required by PEcAn:
list.of.packages <- c('MASS', 'randtoolbox','ggplot2', 'RMySQL', 'gridExtra','xtable')
# of the required packages, these are not yet installed:
new.packages <- list.of.packages[!(list.of.packages %in%
                                   installed.packages(lib = c(.Library, '~/lib/R'))[,"Package"])]
# install required packages:
r <- getOption("repos")
r["CRAN"] <- "http://cran.us.r-project.org"
options(repos = r)
rm(r)

if(length(new.packages)) install.packages(new.packages[new.packages != c('XML')], 
                                          lib = '~/lib/R')
