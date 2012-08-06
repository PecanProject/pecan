read.plot <- function(file){
  dat <- read.csv(file)
  plot <- dat[,which(toupper(names(dat)) == "PLOT")]
  tree <- dat[,which(toupper(names(dat)) == "TREE")]
  spp <- dat[,which(toupper(names(dat)) == "SPECIES")]
  dbh <- dat[,which(toupper(names(dat)) == "DBH")]

  cbind(plot,tree,spp,dbh)
  
}
