##' @title LINKAGES moist function
##' @author Ann Raiho
##'
##' @param kyr current year
##' @param temp.vec average temperature by month for current year
##' @param precip.vec average precipitation by month for current year
##' @param fc field capacity
##' @param dry wilting point
##' @param bgs beginning growing season day of year
##' @param egs end growing season day of year
##' @param plat plot latititude
##' @param clat climate adjustment matrix
##'
##' @description Calculates actual evapotranspiration (AET) for the current year
##'
##' @return aet             actual evapotranspiration (units:mm)
##' @return fj              total number of dry days
##'
moist <- function(kyr,temp.vec,precip.vec,fc,dry,bgs,egs,plat,clat){
  # adjust latitude pointer
  days = c(31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31.)
  lat = round((plat + .5) - 24)

  # initialize water content of soil in january to fc
  xfc = 10*fc
  water = fc

  #initialize thornthwaite parameters
  aet = 0 #actual evapotranspiration
  accpwl = 0 #accumulated potential water loss
  te = 0 #temperature efficiency
  rsave = temp.vec[1]

  for(k in 1:12){
    if(temp.vec[k]<0) temp.vec[k] = 0
    te = te + (.2 * temp.vec[k]) * 1.514 #te = temperature efficiency
  }
  a = .675 * te * 3 - 77.1 * te * 2 + 17920 * te + 492390 #a = exponent of evapotranspiration function
  a = .000001 * a

  #initializ the number of dry days (dd), and current day of year (cday)
  dd = 0
  cday = 15
  nct = 0

  yr = kyr

  #main loop for yearly water balance calculation by month
  for(k in 1:12){
    owater = water
    nct = nct + 1
    if(nct == 2) nct = 0
    #calculate this month's rainfall
    rain = precip.vec[k]
    ttmp = temp.vec[k]
    #calculate potential evapotranspiration (u)
    u = 1.6 * ((10*ttmp/te)^a)*clat[lat,k]
    #calculate potential water loss this month
    pwl = rain - u
    if(pwl < 0){ #if rain satisfies u thes month, don't draw on soil water
      #if rain doesn't satisfy u, add this month's potential water loss to accumulated potential water loss from soil
      accpwl = accpwl + pwl
      xacpwl = accpwl * 10
      #calculate water retained in soil given so much accumulated potential water loss (pastor and post 1984 Candian Journal for Forest Restoration 14:466-467)
      water = fc*(exp((.000461-1.10559/xfc)*(-1*xacpwl)))
      if(water<0) water = 0
      #CSM - change in soil moisture during this  mont
      csm = water - owater
      #calculate actual evapotranspiration (aet) if soil water is drawn down this month
      aet = aet + (rain - csm)
    } else {
      if(water>=fc) water = fc
      csm = water-owater
      #if soil is partially rechared, reduce accumulated potential water loss accordingly
      accpwl = accpwl +csm
      #if soil is completely recharged, reset accumulated potential water loss to zero
      if(water>=fc) accpwl = 0
      #if soil water is not drawn upon, add u to aet
      aet = aet + u
    }
    ocday = cday
    cday = cday + days[k]
    ddi = 0

    #increment the number of dry days interpolating if necessary, truncate at ends of growing season
    if(cday <= bgs) next
    if(ocday >= egs) next
    if(owater >= dry & water >= dry) next
    if(owater>dry & water<dry){
      ddi = days[k] *(dry-water)/(owater-water)
      if(ocday < bgs & cday >bgs) ddi = min(ddi,(cday-bgs))
      if(ocday<egs & cday>egs) ddi = egs - cday + ddi
      if(ddi < 0) ddi = 0
      dd = dd + ddi
    }
    if(owater<dry & water>dry){
      ddi = days[k] * (dry-owater)/(water-owater)
      if(ocday<bgs & cday>bgs) ddi = ocday + ddi - bgs
      if(ddi<0) ddi = 0
      if(ocday<egs & cday >egs) ddi = min(ddi, (egs-ocday))
      dd = dd + ddi
    }
    #save total number of dry days for year
    fj <- dd
    temp.vec[1] = rsave
    #conver aet from cm to mm
    aet <<- aet * 10
  }
  return(list(aet=aet,fj=dd))
}
