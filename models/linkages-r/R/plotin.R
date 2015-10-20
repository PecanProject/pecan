##' @title LINKAGES plotin function
##' @author Ann Raiho
##'
##' @param iplot               number of plots
##' @param basesc              initial humus weight
##' @param basesn              initial humus nitrogen content
##' @param max.ind             maximum number of individuals
##' @param nspec               number of species in simulation
##'
##' @description Sets up initial conditions for LINAKGES
##'
##' @return ntrees             matrix for number of trees for each species
##' @return dbh                matrix for diameter at breast height for each tree
##' @return nogro              used to flag slow growing trees
##' @return ksprt              used to flag trees eligible to sprout
##' @return iage               matrix for age of each tree
##' @return C.mat              matrix for data on litter cohorts
##' @return ncohrt             number of cohorts
##' @return tyl                total yearly litter
##'
plotin <- function(iplot,basesc,basesn,max.ind,nspec){

  ntrees <- matrix(0,1,nspec) #contains number of trees for each species
  dbh <- matrix(0,1,max.ind) #contains diameter at breast height for each tree
  nogro <- matrix(0,1,max.ind) #used to flag slow growing trees
  ksprt <- matrix(0,1,nspec) #used to flag trees eligible to sprout
  iage <- matrix(0,1,max.ind) #contains the age of each tree

  C.mat <- matrix(0,100,15) #contains data on litter cohorts
  C.mat[1,1] <- basesc #initial humus weight
  C.mat[1,2] <- basesn #initial humus nitrogen content
  C.mat[1,5] <- 18 #"litter type" for humus
  ncohrt <- 1 #if ncohrt = 1 only humus is present

  tyl <- matrix(0,1,20) #contains this year's litter

  return(list(ntrees=ntrees,dbh=dbh,nogro=nogro,ksprt=ksprt,
              iage=iage,C.mat=C.mat,ncohrt=ncohrt,tyl=tyl))

}
