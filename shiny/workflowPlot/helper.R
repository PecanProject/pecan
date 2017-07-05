checkAndDownload<-function(packageNames) {
  for(packageName in packageNames) {
    if(!isInstalled(packageName)) {
      install.packages(packageName,repos="http://lib.stat.cmu.edu/R/CRAN") 
    } 
    library(packageName,character.only=TRUE,quietly=TRUE,verbose=FALSE)
  }
}
isInstalled <- function(mypkg){
  is.element(mypkg, installed.packages()[,1])
}
# checkAndDownload(c('plotly','scales','dplyr'))
