##' @title LINKAGES growth function
##' @author Ann Raiho
##'
##' @param max.ind maximum number of individuals
##' @param nspec number of species
##' @param ntrees number of trees of each species
##' @param frt foliage retention time
##' @param slta parameter to calculate crown area from diameter
##' @param sltb parameter to calculate crown area from diameter
##' @param dbh diameter of each tree
##' @param fwt leaf weight per unit crown area
##' @param b2 growth scaling parameter
##' @param b3 growth scaling parameter
##' @param itol shade tolerance code for each species
##' @param g scalar for species maximum diameter increment
##' @param frost minimum January temperature tolerated
##' @param rt current monthly temperature
##' @param degdgf growing degree day growth factor from gmult.R
##' @param smgf soil moisture growth factor from gmult.R
##' @param sngf soil nitrogen growth factor from gmult.R
##' @param iage age of each individual
##' @param nogro flags slow growing individuals
##'
##' @description    GROW CALCULATES DIAMETER GROWTH FOR EACH TREE
##'   BY DECREASING MAXIMAL GROWTH TO THE EXTENT THAT THE MOST
##'   LIMITING RESOURCE IS LESS THAN OPTIMAL.
##'
##' @return dbh diameter of each individual
##' @return ntrees number of trees of each species
##' @return awp aboveground woody production
##'
grow <- function(max.ind,nspec,ntrees,frt,slta,sltb,dbh,fwt, b2,b3, itol,g,
                 degdgf,smgf,sngf,frost,rt,iage,nogro){
  #initialize wood production
  awp = matrix(0,1,max.ind)
  #calculate total number of trees
  ntot = 0
  for(i in 1:nspec) ntot = ntot + ntrees[i]
  if(ntot == 0) break
  if(ntot > max.ind) print("too many trees -- grow")
  #initialize canopy leaf biomass profile
  sumla = matrix(0,1,max.ind)
  #loop for calculating canopy profile
  nl = 1
  for(j in 1:nspec){
    if(ntrees[j]==0) next
    nu = nl + ntrees[j] - 1
    ret = frt[j]
    for(k in nl:nu){
      age = iage[k]
      if(age < ret) ret = age
      #calculate height profile
      iht = ((b2[j]*dbh[k]-b3[j]*dbh[k]^2)/10)+1
      if(iht>700) print("trees too tall")
      #calculate and sum leaf biomass for trees of approx. the same height
      sumla[iht] = sumla[iht] = (((slta[j]+sltb[j]*dbh[k])/2)^2*3.14*fwt[j]*ret)
    }
    nl = nl + ntrees[j]
  }
  #calculate cumulative leaf biomass down through the canopy
  for(j in 1:699){
    j1 = 700-j
    sumla[j1] = sumla[j1] + sumla[j1 + 1]
  }
  #main loop for calculating diameter increment
  nl = 1
  for(i in 1:nspec){
    if(ntrees[i] == 0) next
    nu = nl + ntrees[i] - 1
    for(j in nl:nu){
      #calulate leaf biomass of all taller trees (slar)
      ht = b2[i]*dbh[j] - b3[i]*dbh[j]^2
      iht = ht/10 + 2
      slar = sumla[iht]
      #calculate available light to this tree (% full sunlight)
      al = 1 * exp(-slar/93750)
      #calculate available light multiplier if tree is shade intolerant
      if(itol[i]>=2) {
        algf = 2.24*(1-exp(-1.136*(al-.08)))
      } else {
        algf = 1 - exp(-4.64*(al - .05))
      }
      if(algf<0) algf = 0
      #calculate maximum tree volume
      gr = (137 + .25 * b2[i]^2 / b3[i]) * (.5 * b2[i] / b3[i])
      #calculate diameter increment under optimal conditions
      dncmax = g[i] * dbh[j] * (1 - (137 * dbh[j] + b2[i] * dbh[j]^2 - b3[i] * dbh[j]^3) / gr) / (274 + 3 * b2[i] * dbh[j] - 4 * b3[i] * dbh[j]^2)
      #choose smallest growth multiplier for this tree
      gf = min(algf,smgf[i],sngf[i],degdgf[i])
      #reduce diameter increment to the extent that conditions are less than optimum for growth
      dinc = dncmax*gf
      #check if increment is less than minimum required for growth. if dinc less than 1 mm or 10% of ndcmax or if january temp is less than frost tolerance, flag tree in nogro
      if(dinc < .1 | frost[i] > rt[1]) dinc = 0
      if(dinc >= .1*dncmax) nogro[j] = 0
      if(dinc < .1*dncmax) nogro[j] = nogro[j] - 1
      #calculate woody biomass (kg) before incrementing diameter
      ab1 = .1193 * dbh[j] ^ 2.393
      #increment diameter
      dbh[j] = dbh[j] + dinc

      #if(dbh[j]>10) print(i)
      #calculate woody biomass after incrementing diameter
      ab2 = .1193 * dbh[j] ^ 2.393
      #calculate net increase in woody biomass (aboveground woody production in kg)
      awp[j] = ab2 - ab1
    }
    nl = nl + ntrees[i]
  }
  return(list(ntrees = ntrees, dbh = dbh, awp = awp, nogro = nogro))
}
