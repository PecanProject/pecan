
get.weather <- function(lat, lon, met.nc = met.nc, start.date, end.date, output.dt = 1){
#    if(!is.land(lat, lon)) stop("point is in ocean")
    result <- load.cfmet(lat = lat, lon = lon, met.nc = met.nc, start.date, end.date)
    downscaled.result <- cfmet.downscale.time(cfmet = result, output.dt = output.dt, lat  = lat)
    weather <- cruncep_dt2weather(downscaled.result)
}



is.land <- function(lat, lon){
    Lat <- ncvar_get(nc = met.nc, varid = "lat")
    Lon <- ncvar_get(nc = met.nc, varid = "lon")
    lati <- which.min(abs(Lat-lat))
    loni <- which.min(abs(Lon-lon))
    mask <- ncvar_get(nc = met.nc, varid = "mask",
                      start = c(loni, lati), count = c(1,1))
    return(mask >= 0)
}

get.latlonbox <- function(lati, loni, Lat = Lat, Lon = Lon){
    lat <- c(mean(Lat[lati:(lati-1)]), mean(Lat[lati:(lati+1)]))
    lon <- c(mean(Lon[loni:(loni-1)]), mean(Lon[loni:(loni+1)]))
    return(c(sort(lat), sort(lon)))
}

get.cruncep <- function(lat, lon, start.date, end.date){
    result <- load.cfmet(lat, lon)
    hourly.result <- cruncep_hourly(result, lat = Lat[lati])
    weather <- cruncep_dt2weather(hourly.result)
    return(weather)
}


getNARRforBioCro<-function(lat,lon,year){
    USlayer<-read.table("/home/groups/ebimodeling/met/NARR/ProcessedNARR/NARRindex.txt")
    index <- which.min((lat - USlayer$Latt)^2 + (lon - USlayer$Lonn))
    i <- USlayer$Iindex[index]
    j <- USlayer$Jindex[index]
    filename <- paste("/home/groups/ebimodeling/met/NARR/ProcessedNARR/",year,formatC(i,width=3,flag=0),formatC(j,width=3,flag=0),".RData",sep="")
    load(filename)
    return(dat)
}

##' Simulates the light macro environment
##'
##' Simulates light macro environment based on latitude, day of the year.
##' Other coefficients can be adjusted.
##'
##'
##' @param lat the latitude, default is 40 (Urbana, IL, U.S.).
##' @param DOY the day of the year (1--365), default 190.
##' @param t.d time of the day in hours (0--23), default 12.
##' @param t.sn time of solar noon, default 12.
##' @param atm.P atmospheric pressure, default 1e5 (kPa).
##' @param alpha atmospheric transmittance, default 0.85.
##' @export
##' @return a \code{\link{list}} structure with components:
##' \itemize{
##'  \item{"I.dir"}{Direct radiation (\eqn{\mu} mol \eqn{m^{-2}s^{-1}}}
##'  \item{"I.diff"}{Indirect (diffuse) radiation (\eqn{\mu} mol\eqn{m^{-2}s^{-1}}}
##'  \item{"cos.th"}{cosine of \eqn{\theta}, solar zenith angle.}
##'  \item{"propIdir"}{proportion of direct radiation.}
##'  \item{"propIdiff"}{proportion of indirect (diffuse) radiation.}
##' }
##' @keywords models
lightME <- function(lat=40,DOY=190,t.d=12,t.sn=12,atm.P=1e5,alpha=0.85) {
  
  ## The equations used here can be found in
  ## http://www.life.illinois.edu/plantbio/wimovac/newpage9.htm
  ## The original source is Monteith, 1991
  Dtr <- (pi/180)
  
  omega <- lat * Dtr
  
  delta0 <- 360 * (DOY + 10)/365
  delta <- -23.5 * cos(delta0*Dtr)
  deltaR <- delta * Dtr
  t.f <- (15*(t.d-t.sn))*Dtr
  SSin <- sin(deltaR) * sin(omega)
  CCos <- cos(deltaR) * cos(omega)
  CosZenithAngle0 <- SSin + CCos * cos(t.f)
  CosZenithAngle <- ifelse(CosZenithAngle0 <= 10 ^ -10, 1e-10, CosZenithAngle0)
  
  CosHour <-  -tan(omega) * tan(deltaR)
  CosHourDeg <- (1/Dtr)*(CosHour)
  CosHour <- ifelse(CosHourDeg < -57,-0.994,CosHour)
  Daylength <- 2 * (1/Dtr)*(acos(CosHour)) / 15
  SunUp <- 12 - Daylength / 2
  SunDown <- 12 + Daylength / 2
  SinSolarElevation <- CosZenithAngle
  SolarElevation <- (1/Dtr)*(asin(SinSolarElevation))
  
  PP.o <- 10^5 / atm.P
  Solar_Constant <- 2650
  ## Notice the difference with the website for the eq below
  I.dir <- Solar_Constant * (alpha ^ ((PP.o) / CosZenithAngle))
  I.diff <- 0.3 * Solar_Constant * (1 - alpha ^ ((PP.o) / CosZenithAngle)) * CosZenithAngle
  propIdir <- I.dir / (I.dir+I.diff)
  propIdiff <- I.diff / (I.dir+I.diff)
  
  list(I.dir=I.dir,I.diff=I.diff,cos.th=CosZenithAngle,propIdir=propIdir,propIdiff=propIdiff)
  
}
