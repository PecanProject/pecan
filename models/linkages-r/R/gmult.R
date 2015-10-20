##' @title LINKAGES gmult function
##' @author Ann Raiho
##'
##' @param bgs   beginning of growing season day of year
##' @param egs    end of growing season day of year
##' @param availn   available nitrogen
##' @param degd     total growing degree days
##' @param dmin degree day minimum for each species
##' @param dmax degree day maximum for each species
##' @param d3 drought tolerance (fraction of the growing season)
##' @param cm1 parameter for nitrogen growth multipliers
##' @param cm2 parameter for nitrogen growth multipliers
##' @param cm3 parameter for nitrogen growth multipliers
##' @param cm4 parameter for nitrogen growth multipliers
##' @param cm5 parameter for nitrogen growth multipliers
##' @param nspec     number of species
##' @param fj         number of dry days
##'
##' @description    GMULT CALCULATES DEGREE DAY, SOIL MOISTURE, AND SOIL
##'                 NITROGEN MULTIPLIERS USED IN SUBROUTINES BIRTH AND GROW BASED
##'                 ON ON DEGD (SUPPLIED BY TEMPE), FJ (SUPPLIED BY MOIST), AND
##'                 AVAILN (SUPPLIED BY DECOMP), RESPECTIVELY.
##'
##' @return smgf soil moisture growth factor
##' @return sngf soil nitrogen growth factor
##' @return degdgf growing degree day growth factor
##' @return availn available nitrogen
##'
gmult <- function(egs,bgs,availn,degd,dmin,dmax,d3,fj,cm1,cm3,cm2,cm4,cm5,nspec){
  tgs = egs - bgs + 1

  availn = availn + .005
  avlmc = -170 + 4*(availn*1000)

  degdgf = matrix(0,1,nspec)
  smgf = matrix(0,1,nspec)
  sngf = matrix(0,1,nspec)

  for(i in 1:nspec){
    degdgf[i] =(4 * (degd - dmin[i]) * (dmax[i] - degd)) / ((dmax[i] - dmin[i]) ^ 2) #Botkin 1972 EQ#10

    if(degdgf[i] < 0 ) degdgf[i] = 0
    if(degdgf[i] != 0){
      drout = d3[i] * tgs
      if(drout < fj) drout = fj
      smgf[i] = sqrt((drout - fj)/drout)
      if(smgf[i] != 0){
        conn = cm1[i] * (1 - 10^((-1*cm3[i])*(avlmc+cm2[i])))
        sngf[i] = cm4[i] + cm5[i] * conn
        if(sngf[i] < 0) sngf[i] = 0
      }
    }
  }

  return(list(smgf=smgf,sngf=sngf,degdgf=degdgf,availn=availn))

}
