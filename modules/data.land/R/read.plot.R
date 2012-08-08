read.plot <- function(file){
  dat <- read.csv(file)
  plot <- dat[,which(toupper(names(dat)) == "PLOT")]
  tree <- dat[,which(toupper(names(dat)) == "TREE")]
  spp <- as.character(dat[,which(toupper(names(dat)) == "SPECIES")])
  dbh <- dat[,which(toupper(names(dat)) == "DBH")]

  data.frame(plot=plot,tree=tree,spp=spp,dbh=dbh)
  
}
