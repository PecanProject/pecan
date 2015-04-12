## Create FORTRAN Data Module from list object

f.data.module <- function(dat, types, modname, fname){
	if(!is.list(dat)) dat <- as.list(dat)
	if(is.null(names(dat))) names(dat) <- sprintf("obj%d", 1:length(dat))
	obj.names <- names(dat)
	ld <- length(dat)
	
	write.strings <- list()
	write.strings[1] <- paste0("MODULE MOD_", modname)
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
	write.strings[j] <- "END MODULE"
	
	write("! Automatically generated module", fname)
	for(s in write.strings) write(s, fname, append=TRUE)
}


	
		
	