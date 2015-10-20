##' @title LINKAGES output function
##' @author Ann Raiho
##'
##' @param availn available nitrogen
##' @param tyln leaf litter N content from "decomp.r"
##' @param nspec number of species
##' @param frt,slta,sltb,fwt species specific parameters
##' @param iage age of each individual
##' @param dbh diameter of each individual
##' @param tyl total yearly litter
##' @param max.ind maximum number of individual
##' @param ntrees number of trees of each species
##' @param awp aboveground woody production
##'
##' @description    OUTPUT coverts model variables into ecosystem variables of interest
##'
##' @return atot total number of trees per ha
##' @return tbar total aboveground biomass
##' @return tyln leaf litter N content
##' @return tynap total aboveground production
##' @return availn available N
##' @return bar specieis biomass t/ha
##'
output <- function(availn,tyln,nspec,frt,iage,slta,sltb,dbh,fwt,tyl,max.ind,ntrees,awp){

  #initialization
  area = 0 #leaf area
  folw = 0 #leaf biomass
  availn = availn*1000 #available nitrogen from "gmult.r"
  tbar = 0 #total aboveground biomass
  tawp = 0 #total aboveground woody production
  ntot = 0 #number of trees
  tyln = tyln*1000 #leaf litter N content from "decomp.r"

  bar = numeric(nspec)

  #calculate spp biomass, total biomass, total number of stems, leaf area, and total woody production
  nl = 1
  for(i in 1:nspec){
    bar[i] = 0
    if(ntrees[i]==0) next
    nu = nl + ntrees[i] - 1
    ret = frt[i]
    for(j in nl:nu){
      age = iage[j]
      #calculate leaf biomass (kg/tree)
      folw = ((slta[i]+sltb[i]*dbh[j])/2)^2 * 3.14 * fwt[i] * ret * .001
      #calculate species biomass (kg/plot)
      bar[i] = bar[i] + .1193 * dbh[j]^2.393 + folw
      #calculate leaf area index
      area = area +1.9283295 * 10^-4 * dbh[j]^2.129
      #calculate woody production (kg/plot)
      tawp = tawp + awp[j]
    }
    #calculate total aboveground biomass (kg/plot)
    tbar = tbar + bar[i]
    nl = nu+1
    #calculate total number of trees per plot
    ntot = ntot + ntrees[i]
    if(ntot > max.ind) print("too many trees -- output")
  }
  #convert number of treees per plot to number per ha
  atot = ntot
  atot = atot*12
  #convert total aboveground biomass and woody production to t/ha
  tbar = tbar * .012
  tawp = tawp * .012
  #calculate total aboveground production
  tynap = tawp + tyl[17]
  #convert spp biomass to t/ha
  for(j in 1:nspec){
    bar[j] = bar[j] * .012
  }

  return(list(atot=atot,tbar=tbar,tyln=tyln,tynap=tynap,availn=availn,
              bar=bar))

}
