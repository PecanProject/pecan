##' Simple, Fast Daily to Hourly Climate Downscaling
##'
##' Based on weach family of functions but 5x faster than weachNEW,
##' and requiring metric units (temperature in celsius, windspeed in kph,
##' precip in mm, relative humidity as fraction)
##' @title weachDT
##' @param X data table with climate variables
##' @param lati latitude (for calculating solar radiation)
##' @export
##' @return weather file for input to BioGro and related crop growth functions
##' @author David LeBauer
weachDT <- function(X, lati){

    tint <- 24
    tseq <- 0:23
    ## Solar Radiation

    if("day" %in% colnames(X)) setnames(X, "day", "doy")
    setkeyv(X, c("year", "doy"))

    solarR <-  X[,list(solarR = rep((0.12 * dswrf.MJ) * 2.07 * 10^6 / 3600, each = tint) ), by = c("year", "doy")]
    
    light <- X[,lightME(DOY = doy, t.d = tseq, lat = 40),
                      by = c("year", "doy")]

    light$Itot <- light[,list(I.dir + I.diff)]
    resC2 <- light[, list(resC2 = (Itot - min(Itot)) / max(Itot)), by = c("year", "doy")] 
    
    SolarR <- cbind(resC2, solarR)[,list(SolarR = solarR * resC2)]

    ## Temperature
    Temp <- X[,list(Temp = tmin + (sin(2*pi*(tseq-10)/tint) + 1)/2 * (tmax - tmin)),
              by = c("year", "doy")][,list(Temp)]

    ## Relative Humidity
    rhscale <- (cos(2 * pi * (tseq - 10) / tint) + 1) / 2
    RH <- X[,list(RH = rhmin + rhscale * (rhmax - rhmin)), by = c("year", "doy")][,list(RH)]

    ## Wind Speed
    WS <- rep(X$wnd, each = tint)

    ## Precipitation
    precip <- rep(X$precip/tint, each = tint)

    ## Hour
    time <- X[,list(hour = tseq), by = c("year", "doy")]
    
    ans <- cbind(time, SolarR, Temp, RH, WS, precip)
    return(ans)
}
    
##' Simulates the light macro environment
##'
##' Simulates light macro environment based on latitude, day of the year.
##' Other coefficients can be adjusted.
##'
##' The equations used here can be found in
##' http://www.life.illinois.edu/plantbio/wimovac/newpage9.htm
##' The original source is Monteith, 1991
##'
##' @param lat the latitude, default is 40 (Urbana, IL, U.S.).
##' @param DOY the day of the year (1--365), default 190.
##' @param t.d time of the day in hours (0--23), default 12.
##' @param t.sn time of solar noon, default 12.
##' @param atm.P atmospheric pressure, default 1e5 (kPa).
##' @param alpha atmospheric transmittance, default 0.85.
##' @export
##' @return a \code{\link{list}} structure with components
##' \begin{itemize}
##' \item I.dir Direct radiation (\eqn{\mu} mol \eqn{m^{-2}}
##' \eqn{s^{-1}}).
##' \item  I.diff Indirect (diffuse) radiation (\eqn{\mu} mol \eqn{m^{-2}}
##' \eqn{s^{-1}}).
##' \item cos.th cosine of \eqn{\theta}, solar zenith angle.
##' \item propIdir proportion of direct radiation.
##' \item propIdir proportion of indirect (diffuse) radiation.
##' @keywords models
##' @author Fernando Miguez
##' @examples
##' 
##'
##' 
##' ## Direct and diffuse radiation for DOY 190 and hours 0 to 23
##'
##' res <- lightME(t.d=0:23)
##'
##' xyplot(I.dir + I.diff ~ 0:23 , data = res,
##' type='o',xlab='hour',ylab='Irradiance')
##'
##' xyplot(propIdir + propIdiff ~ 0:23 , data = res,
##' type='o',xlab='hour',ylab='Irradiance proportion')
##'
##' plot(acos(lightME(lat = 42, t.d = 0:23)$cos.th) * (1/dtr))
##'
lightME <- function(lat=40,DOY=190,t.d=12,t.sn=12,atm.P=1e5,alpha=0.85) {

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


