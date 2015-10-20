##' @title LINKAGES kill function
##' @author Ann Raiho
##'
##' @param max.ind maximum number of individuals
##' @param nspec number of species
##' @param ntrees number of trees of each species
##' @param slta parameter to calculate crown area from diameter
##' @param sltb parameter to calculate crown area from diameter
##' @param agemx max age
##' @param sprtmn minimum diameter for a stump to sprout
##' @param sprtmx maximum diameter for a stump to sprout
##' @param tl leaf litter quality class
##' @param rtst root-shoot ratio for each species
##' @param fwt leaf weight per unit crown area
##' @param frt foliage retention time
##' @param ncohrt number of cohorts
##' @param nogro flags slow growing individuals
##' @param iage age of each individual
##' @param dbh diameter of each individual
##' @param ksprt flags if stump can sprout
##'
##' @description    KILL KILLS TREES BY AGE DEPENDENT MORTALITY (ONLY 1%
##'   REACH MAXIMUM AGE) AND AGE INDEPENDENT MORTALITY (PROBABILITY OF
##'   SURVIVING 10 CONSECUTIVE YEARS OF SLOW GROWTH (SEE GROW) = 1%).
##'   DECISIONS ON WHETHER OR NOT TO KILL A TREE ARE PARTLY BASED ON
##'   RANDOM NUMBERS SUPPLIED BY rand.
##'   KILL ALSO CALCULATES LITTER AMOUNTS, WHICH ARE DECAYED IN
##'   SUBROUTINE DECOMP.
##'
##' @return dbh diameter of each individual
##' @return ntrees number of trees of each species
##' @return iage age of each individual
##' @return nogro flags individuals growing slowly
##' @return ncohrt number of cohorts
##' @return tyl total yearly litter,
##' @return ksprt flags stumps that could sprout
##'
kill <- function(nspec, ntrees,slta,sltb,dbh,agemx,ksprt,sprtmn,sprtmx,iage,
                 nogro,tl,rtst,fwt,max.ind,frt,ncohrt){
  knt = 0
  #initialize litter
  tyl = matrix(0,1,20)

  #initialize plot basal area
  ba = 0

  #begin main killing loop
  for(i in 1:nspec){
    if(ntrees[i]==0) next
    nl = knt + 1
    nu = ntrees[i] + knt
    for(k in nl:nu){
      #calculate leaf production (tree/ha)
      folw = ((slta[i] + sltb[i] * dbh[k]) / 2) ^ 2 * 3.14 * fwt[i] * .000012

      #calculate basal area
      ba = ba + .0314 * (dbh[k]*.5) ^ 2

      #kill trees based on probability that only 1% reach max age
      yfl = runif(1,0,1) #this changes everything... pexp(agemx[i],1/(agemx[i]/2)) 4.605/agemx[i]
      if(yfl <= 4.605/agemx[i]) {
        ntrees[i] = ntrees[i] - 1

        #check to see if dead tree can stump sprout increment skprt if tree can sprout
        if(dbh[k]>sprtmn[i] & dbh[k]<sprtmx[i]) ksprt[i] = ksprt[i] + 1

        #calculate woody litter in t/ha
        bd = .60
        if(dbh[k] <= .1) tyl[14] = tyl[14] + bd * (.00143 * dbh[k] ^ 2.393)
        if(dbh[k] > .1) tyl[15] = tyl[15] + bd * (.00143 * dbh[k] ^ 2.393)

        #flag dead trees
        dbh[k] = -1
       } else {

          if(nogro[k]<=-2){
            yfl = runif(1,0,1)
            if(yfl <= .368){
            ntrees[i] = ntrees[i] - 1

            #check to see if dead tree can sump sprout increment skprt if tree can sprout
            if(dbh[k]>sprtmn[i] & dbh[k]<sprtmx[i]) ksprt[i] = ksprt[i] + 1

            #calculate woody litter in t/ha
            bd = .60
            if(dbh[k]<=.1) tyl[14] = tyl[14] + bd * (.00143 * dbh[k] ^ 2.393)
            if(dbh[k]>.1) tyl[15] = tyl[15] + bd * (.00143 * dbh[k] ^ 2.393)

            #flag dead trees
            dbh[k] = -1
            }
          }
      }
      #calculate leaf litter by quality class in t/ha if the tree is slow growing but didn't di, leaf litter is halved
      #if the tree died, total leaf biomass is returned to the soil
      L = tl[i]
      if(nogro[k] == -2 & dbh[k] > -1) folw = folw*.5
      if(dbh[k] < 0) folw = folw * frt[i]
      tyl[L] = tyl[L] + folw
      #calculate root litter (t/ha)
      tyl[13] = tyl[13] + 1.3 * folw * rtst[i]
    }
    knt = nu
  }
  #calculate total leaf litter (t/ha)
  tyl[17] = tyl[17] + sum(tyl[1:12])

  #calculate twig litter in t/ha
  tyl[16] = ba/333

  #calculate total litter (t/ha)
  tyl[18] = sum(tyl[13:17])

  #rewrite diameters and ages to eliminate dead trees
  k = 0
  ntot = 0
  for(i in 1:max.ind){
    if(dbh[i]==0) {
      ntot = k
      next
    }
    if(dbh[i]<0){
      next
    }
    k = k+1
    dbh[k] = dbh[i]
    iage[k] = iage[i]
    nogro[k] = nogro[i]
    ntot = k
  }

  if(ntot==0) print("here")
  ntot1 = k+1
  if(ntot1 > max.ind) print("too many trees -- kill")

  #eliminate dead trees
  for(i in ntot1:nu){
    dbh[i] = 0
    iage[i] = 0
    nogro[i] = 0
  }

  return(list(ntrees = ntrees, dbh = dbh, iage = iage, nogro = nogro, ncohrt = ncohrt,
              tyl = tyl, ksprt = ksprt))

}
