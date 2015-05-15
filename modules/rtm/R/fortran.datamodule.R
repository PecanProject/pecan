#'@name f.data.module
#'@title List to FORTRAN data module
#'@author Alexey Shiklomanov
#'@details For models with large constants (e.g. absorption features in the
#'      PROSPECT model), it may be preferable to store these in FORTRAN90
#'      modules. However, manually creating and formatting these files is 
#'      tedious. This script allows you to automatically generate module files
#'      from R lists. It automatically interprets the object lengths as array
#'      dimensions (only vectors are supported right now -- higher dimension
#'      arrays may be in the future) and splits long data into rows of 10.
#'      Currently, only numeric data are supported (i.e. no characters).
#'@param dat Named list object for creating module. List names will be used
#'      to initialize FORTRAN variabiles.
#'@param types Character vector of FORTAN types (e.g. real*8, integer)
#'@param modname Name of the module. We suggest the format 'MOD_yourmodname'.
#'@param fname Output file name. Defaults to 'yourmodname.f90'
#'@example
#'      w <- 3.2
#'      x <- 1:5
#'      y <- 6:15
#'      z <- seq(exp(1), pi, length.out=42)
#'      l <- list(x=x, y=y, z=z) ## NOTE that names must be explicitly declared
#'      l.types <- c("real","integer", "real*4", "real*8")
#'      f.data.module(l, l.types, "testmod")
#'@example
#'      x <- runif(10)
#'      y <- rnorm(10)
#'      z <- rgamma(10, 3)
#'      d <- data.frame(x,y,z) ## NOTE that data.frames are just named lists
#'      d.types <- rep("real*8", ncol(d))
#'      f.data.module(d, d.types, "random") 

f.data.module <- function(dat, types, modname, fname = paste0(modname,".f90")){
    if(!is.list(dat)) dat <- as.list(dat)
    if(is.null(names(dat))) stop("'dat' must have names")
    obj.names <- names(dat)
    ld <- length(dat)

    write.strings <- list()
    write.strings[1] <- paste0("MODULE", modname)
    write.strings[2] <- "implicit.none"
    write.strings[3] <- "integer :: i"
    j <- length(write.strings)
    for(i in 1:ld){
        d <- dat[[i]]
        j <- j+1
        write.strings[j] <- sprintf("%s, dimension(%d) :: %s",
                                    types[i], length(d), obj.names[i])
    }

    for(i in 1:ld){
        d <- dat[[i]]
        dc <- sprintf("%g", d)
        d10 <- length(d) %/% 10
        if(length(d) %% 10 != 0) d10 <- d10 + 1
        dmin <- 1
        for(i10 in 1:d10){
            dmax <- min(dmin + 9, length(d))
            j <- j+1
            write.strings[j] <- sprintf("DATA (%s(i),i=%d,%d)/&",
                                        obj.names[i], dmin, dmax)
            j <- j+1
            write.strings[j] <- paste0(paste(dc[dmin:dmax], collapse=","),"/")
            j <- j+1
            write.strings[j] <-""
            dmin <- dmax + 1
        }
        j <- j+1
        write.strings[j] <- ""
    }
    j <- j+1
    write.strings[j] <- sprintf("END MODULE %s", modname)

    write("! Automatically generated module", fname)
    for(s in write.strings) write(s, fname, append=TRUE)
}

