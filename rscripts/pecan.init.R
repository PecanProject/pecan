
# these packages are required by PEcAn:
list.of.packages <- c('XML', 'MASS', 'randtoolbox','rjags', 'ggplot2', 'RMySQL', 'gridExtra','xtable')
# of the required packages, these are not yet installed:
new.packages <- list.of.packages[!(list.of.packages %in%
                                   installed.packages(lib = c(.Library, '~/lib/R'))[,"Package"])]
# install required packages:
if(length(new.packages)) install.packages(new.packages[new.packages != c('XML', 'rjags')], 
			 lib = '~/lib/R', 
			 repos = "http://lib.stat.cmu.edu/R/CRAN")
if('XML' %in% 'new.packages') system('apt-get install r-core-xml')
