
## packages available from debian repository installed by install.dependencies.sh
# packages required by PEcAn:
list.of.packages <- c('randtoolbox','ggplot2', 'RMySQL', 'gridExtra','xtable','chron')
# of the required packages, these are not yet installed:
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# install required packages:
r <- getOption("repos")
r["CRAN"] <- "http://cran.us.r-project.org"
options(repos = r)
rm(r)


cat(paste('packages to be installed \n', new.packages))



if(length(new.packages)) install.packages(new.packages)
