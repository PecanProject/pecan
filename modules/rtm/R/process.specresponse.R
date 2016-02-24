process.licor.rsr <- function(csv.path) {
    licor.dat <- read.csv(csv.path)
    blue <- licor.dat[1:2]
    blue <- blue[complete.cases(blue),]
    blue.rsr <- interpolate.rsr(blue)
    red <- licor.dat[3:4]
    red <- red[complete.cases(red),]
    red.rsr <- interpolate.rsr(red)
    farred <- licor.dat[5:6]
    farred <- farred[complete.cases(farred),]
    farred.rsr <- interpolate.rsr(farred)
    licor.rsr <- cbind(400:2500-399, blue.rsr, red.rsr, farred.rsr)
    licor.rsr <- licor.rsr[400:800-399,]
    colnames(licor.rsr) <- c("index", "blue", "red", "farred")
    return(licor.rsr)
}

interpolate.rsr <- function(rsr, wl.start = 400, wl.end = 2500, zero.threshold = 1e-3){
    wl.int <- seq(floor(min(rsr[[1]])), 
                  ceiling(max(rsr[[1]])))
    wl.offset <- wl.start - 1
    wl.length <- wl.end - wl.offset
    wl.index <- wl.int - wl.offset
    rsr.full <- numeric(wl.length)
    rsr.spline <- splinefun(x = rsr[[1]], y = rsr[[2]])
    rsr.interpolated <- rsr.spline(wl.int)
    rsr.interpolated[rsr.interpolated < zero.threshold] <- 0
    rsr.full[wl.index] <- rsr.interpolated
    return(rsr.full)
}
    
