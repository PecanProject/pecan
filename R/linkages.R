##' @title LINKAGES main function
##' @author Ann Raiho
##'
##' @param iplot    PEcAn settings object
##' @param nyear          data.frame of initial condition sample (nens X nstate)
##' @param nspec       data.frame of model parameter sample (nense X nstate)
##' @param fc         data.frame of observations with columns: mean, sd
##' @param dry         data.frame of observations with columns: mean, sd
##' @param bgs         data.frame of observations with columns: mean, sd
##' @param egs         data.frame of observations with columns: mean, sd
##' @param max.ind         data.frame of observations with columns: mean, sd
##' @param plat         data.frame of observations with columns: mean, sd
##'
##' @description Main function for running all LINKAGES subroutines
##'
##' @return several things ---> should this be different
##'
linkages <- function(iplot, nyear, nspec, fc, dry, bgs, egs, max.ind, plat){
  #Storage #this should probably be outside the function too?

  tstem = matrix(0,nyear,iplot) #number of stems
  tab = matrix(0,nyear,iplot) #total aboveground biomass
  fl = matrix(0,nyear,iplot) #leaf litter
  totl = matrix(0,nyear,iplot) #leaf litter N
  tnap = matrix(0,nyear,iplot) #net aboveground production
  avln = matrix(0,nyear,iplot) #available nitrogen
  cn = matrix(0,nyear,iplot) #humus C:N ratio
  sco2c = matrix(0,nyear,iplot) #soil co2 evolution
  som = matrix(0,nyear,iplot) #soil organic matter
  aet.save = matrix(0,nyear,iplot)
  ncohrt.save = matrix(0,nyear,iplot)
  tyl.save = array(0,dim=c(20,nyear,iplot))
  ntrees.birth <- array(0,dim=c(nspec,nyear,iplot))
  ntrees.grow <- array(0,dim=c(nspec,nyear,iplot))
  ntrees.kill <- array(0,dim=c(nspec,nyear,iplot))
  bar <- array(0,dim=c(nspec,nyear,iplot))
  nogro.save <- array(0,dim=c(max.ind,nyear,iplot))
  dbh.save <- array(0,dim=c(max.ind,nyear,iplot))
  iage.save <- array(0,dim=c(max.ind,nyear,iplot))

  temp.mat <- matrix(c(-8.6,-7.6,-1.9,6.9,13.7,19,21.6,20.5,15.9,9.6,.8,-6.1),nyear,12,byrow = TRUE)
  precip.mat <- matrix(c(2.9,2.7,4.2,7,9.2,11.2,8,8.9,8.9,5.7,5.5,2.9),nyear,12,byrow=TRUE)

  for(k in 1:iplot){ #loop over plots

    plotin.out <- plotin(iplot = k, basesc = 74, basesn = 1.64, max.ind = max.ind,
                         nspec = nspec) # initializes storage matrices with zeros for each plot

    ncohrt <- unlist(plotin.out$ncohrt)
    tyl <- unlist(plotin.out$tyl)
    C.mat <- unlist(plotin.out$C.mat)
    ntrees <- unlist(plotin.out$ntrees)
    dbh <- unlist(plotin.out$dbh)
    nogro <- unlist(plotin.out$nogro)
    ksprt <- unlist(plotin.out$ksprt)
    iage <- unlist(plotin.out$iage)

    for(i in 1:nyear){

      tempe.out <- tempe(temp.vec = temp.mat[i,]) #calculates degree days for the year

      degd = unlist(tempe.out$degd)

      moist.out <- moist(kyr = i, temp.vec = temp.mat[i,], precip.vec = precip.mat[i,],
            fc = fc, dry = dry, bgs = bgs, egs = egs, plat = plat, clat = clat) #calculates aet

      aet <- unlist(moist.out$aet)
      fj <- unlist(moist.out$fj)

      decomp.out <- decomp(fdat = fdat, aet = aet,
                           ncohrt = ncohrt, fc = fc, dry = dry,
                           tyl = tyl, C.mat = C.mat)

      ff <- unlist(decomp.out$ff)
      availn <- unlist(decomp.out$availn)
      tyln <- unlist(decomp.out$tyln)
      hcn <- unlist(decomp.out$hcn)
      sco2 <- unlist(decomp.out$sco2)
      ncohrt <- unlist(decomp.out$ncohrt)
      C.mat <- unlist(decomp.out$C.mat)

      gmult.out <- gmult(bgs = bgs, egs = egs, availn = availn,
                        degd = degd, dmin = spp.params$DMIN,
                        dmax = spp.params$DMAX, d3 = spp.params$D3, fj = fj,
                        cm1 = spp.params$CM1, cm3 = spp.params$CM3, cm2 = spp.params$CM2,
                        cm4 = spp.params$CM4, cm5 = spp.params$CM5, nspec = nspec)

      smgf <- unlist(gmult.out$smgf)
      sngf <- unlist(gmult.out$sngf)
      degdgf <- unlist(gmult.out$degdgf)
      availn <- unlist(gmult.out$availn)

     birth.out <- birth(nspec = nspec, ntrees = ntrees, frt = spp.params$FRT, iage = iage,
            slta = spp.params$SLTA, sltb = spp.params$SLTB, dbh = dbh,
            fwt = spp.params$FWT, switch.mat = switch.mat,
            degd = degd, dmin = spp.params$DMIN, dmax = spp.params$DMAX,
            frost = spp.params$FROST, rt = temp.mat[i,], itol = spp.params$ITOL,
            mplant = spp.params$MPLANT, nogro = nogro,
            ksprt = ksprt, sprtnd = spp.params$SPRTND, max.ind = max.ind, smgf=smgf,
            degdgf = degdgf)

     ntrees <- unlist(birth.out$ntrees)
     ntrees.birth[,i,k] <- unlist(birth.out$ntrees)
     dbh <- unlist(birth.out$dbh)
     nogro <- unlist(birth.out$nogro)
     ksprt <- unlist(birth.out$ksprt)
     iage <- unlist(birth.out$iage)

     grow.out <- grow(max.ind = max.ind, nspec = nspec, ntrees = ntrees, frt = spp.params$FRT, slta = spp.params$SLTA,
           sltb = spp.params$SLTB, dbh = dbh, fwt = spp.params$FWT, b2 = spp.params$B2,
           b3 = spp.params$B3, itol =spp.params$ITOL, g = spp.params$G, degdgf = degdgf,
           smgf = smgf, sngf= sngf,frost = spp.params$FROST, rt = temp.mat[i,], iage = iage,
           nogro=nogro)

     ntrees <- unlist(grow.out$ntrees)
     ntrees.grow[,i,k] <- unlist(grow.out$ntrees)
     dbh <- unlist(grow.out$dbh)
     awp <- unlist(grow.out$awp)
     nogro <- unlist(grow.out$nogro)


    kill.out<- kill(nspec = nspec, ntrees= ntrees,slta = spp.params$SLTA, sltb = spp.params$SLTB,
           dbh = dbh, agemx = spp.params$AGEMX, ksprt = ksprt,
           sprtmn = spp.params$SPRTMN, sprtmx = spp.params$SPRTMX, iage  = iage,
           nogro  = nogro,tl = spp.params$TL,rtst = spp.params$RTST, fwt = spp.params$FWT,
           max.ind = max.ind, frt = spp.params$FRT,ncohrt = ncohrt)

    ntrees <- unlist(kill.out$ntrees)
    ntrees.kill[,i,k] <- unlist(kill.out$ntrees)
    dbh <- unlist(kill.out$dbh)
    nogro <- unlist(kill.out$nogro)
    ksprt <- unlist(kill.out$ksprt)
    iage <- unlist(kill.out$iage)
    ncohrt <- unlist(kill.out$ncohrt)
    tyl <- unlist(kill.out$tyl)
    tyl.save[,i,k] <- unlist(kill.out$tyl)

    output.out <- output(availn = availn, tyln = tyln, nspec = nspec, frt=spp.params$FRT,
                         iage = iage,slta = spp.params$SLTA, max.ind = max.ind,
                         sltb = spp.params$SLTB,dbh = dbh,fwt = spp.params$FWT,tyl = tyl,
                         ntrees=ntrees,awp=awp)

    tstem[i,k] = unlist(output.out$atot) #number of stems
    tab[i,k] = unlist(output.out$tbar) #total aboveground biomass
    fl[i,k] = unlist(kill.out$tyl)[17] #leaf litter
    totl[i,k] = unlist(output.out$tyln) #leaf litter N
    tnap[i,k] = unlist(output.out$tynap) #net aboveground production
    avln[i,k] = unlist(gmult.out$availn) #available nitrogen
    cn[i,k] = unlist(decomp.out$hcn) #humus C:N ratio
    sco2c[i,k] = unlist(decomp.out$sco2) #soil co2 evolution
    som[i,k] = unlist(decomp.out$ff[19,2]) #soil organic matter
    bar[,i,k] = unlist(output.out$bar) #species biomass
    aet.save[i,k] = aet
    nogro.save[,i,k] = unlist(kill.out$nogro)
    dbh.save[,i,k] = unlist(kill.out$dbh)
    iage.save[,i,k] = unlist(kill.out$iage)

    print(paste("year = ",i))
    }
    print(paste("PLOT = ",k))
  }
  return(list(ntrees.birth=ntrees.birth, ntrees.kill = ntrees.kill, tstem=tstem,
              tab=tab,fl=fl,totl=totl,tnap=tnap,avln=avln,cn=cn,sco2c=sco2c,
              som=som,bar=bar,aet.save=aet.save,nogro.save=nogro.save,
              dbh.save=dbh.save,iage.save=iage.save))
}
