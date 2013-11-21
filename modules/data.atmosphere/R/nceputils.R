ncep_nc2dt <- function(lati, loni){

    currentlat <- round(Lat[lati], 2)
    currentlon <- round(Lon[loni], 2) - 180
    result <- list()
    
    vars <- list(year = year, doy = doy)
    for(var in names(ncep.nc$var)){
        vars[[var]] <- ncvar_get(nc = ncep.nc,
                                 varid = var,
                                 start = c(loni, lati, 1),
                                 count = c(1, 1, length(time.idx)))
    }
    
    result <- as.data.table(vars)
    return(result)   
}

qair2rh <- function(qair, temp, press = 1013.25){
    es <-  6.112 * exp((17.67 * temp)/(temp + 243.5))
    e <- qair * press / (0.378 * qair + 0.622)
    rh <- e / es
    rh[rh > 1] <- 1
    rh[rh < 0] <- 0

    return(rh)
}

## qc functions restricting to "valid range" given in .nc meta-data
qctemp   <- function(x) ifelse(x > 400 | x < 100, mean(x[x < 400 & x > 100]), x)
qcsolar  <- function(x) ifelse(x<0, 0, ifelse(abs(x) > 1300, mean(x[x < 1300]), x))
qcwind   <- function(x) ifelse(abs(x) > 102, mean(abs(x[x < 102])), x)
qcprecip <- function(x) ifelse(x > 0.005 | x < 0 , mean(x[x < 0.005 & x >0]), x)
qcrh     <- function(x) {
    ifelse(x > 100 | x < 0, mean(x[x < 100 & x>0]), x) ## using logical range (0-100) rather than "valid range (-25-125)"
}
qcshum     <- function(x){
    x <- ifelse(x > 100 | x < 0, mean(x[x < 0.6553 & x > 0]), x)
    x[is.na(x)] <- mean(x, na.rm = TRUE)
}
