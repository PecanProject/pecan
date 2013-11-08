## Code to calculate lapse rates based on PRISM reanalysis
## used prism rather than raw station data both to simplify analysis
## and to average over both station location biases
## and PRISM corrections for other topographic effects (aspect, distance to coast, etc.)
## will hopefully eventually add other topo effects to clim corrections

`flip` <- function(x){t(x[nrow(x):1,])}

`read.grid` <- function(file,list=FALSE){
  header <- scan(file=file,what=list(name="character",val="double"),nlines=6,quiet=TRUE)
  header$val <- as(header$val,"numeric")
  #[(charmatch(c("xll","yll"),substring(header$name,1,3)))]
  grid <- flip(unname(as.matrix(read.table(file,skip=6,header=F,na.strings="-9999"))))
  if (list) return(list(grid=grid,hname=header$name,hval=header$val))
  return(grid)
}

plot.grid <- function(grid,xlim=NULL,ylim=NULL){
  xmin <- grid$hval[grid$hname == "xllcorner"]
  ymin <- grid$hval[grid$hname == "yllcorner"]
  nrow <- grid$hval[grid$hname == "nrows"]
  ncol <- grid$hval[grid$hname == "ncols"]
  res <- grid$hval[grid$hname == "cellsize"]
  if(is.null(xlim)) {    xlim <- c(xmin,xmin+ncol*res)}
  if(is.null(ylim)) {    ylim <- c(ymin,ymin+nrow*res)}
  xscale <- seq(xmin,xmin+ncol*res,length=ncol)
  yscale <- seq(ymin,ymin+nrow*res,length=nrow)
  cols <- findInterval(xlim,xscale)
  rows <- findInterval(ylim,yscale)
  r <- rows[1]:rows[2]
  c <- cols[1]:cols[2]
  #  print(list(xmin,ymin,nrow,ncol,res,xlim,ylim))
  image(xscale[c],yscale[r],grid$grid[c,r])
}

slm <- function(x,y){  ## simple linear model

  ##reduce data
  valid <- apply(cbind(is.na(x),is.na(y)),1,sum) == 0
  x <- x[valid]
  y <- y[valid]
  
  xbar <- mean(x)
  ybar <- mean(y)
  ssx <- sum((x-xbar)^2)
  spxy <- sum((x-xbar)*(y-ybar))
  b1 <- spxy/ssx
  b0 <- ybar-b1*xbar
  return(c(b0,b1))
}

library(maps)

## load up PRISM dem (terrain map) at 800m resolution
dem <- read.grid("us_30s_dem.asc",TRUE)
xlim <- c(-98,-67)
ylim <- c(25,50)
plot.grid(dem,xlim=xlim,ylim=ylim)
map("state",add=TRUE)

## load up mean climate map
annual <- read.grid("us_ppt_1971_2000.14",TRUE)
plot.grid(annual,xlim=xlim,ylim=ylim)
map("state",add=TRUE)
monthly <- list()
for(i in 1:12){
  fname <- paste("us_ppt_1971_2000.",i,sep="")
  monthly[[i]] <- read.grid(fname)
}

## define coarse grid
nx <- xlim[2]-xlim[1]
ny <- ylim[2]-ylim[1]
pbar <- matrix(NA,nx,ny)
pdiff <- NA*annual$grid

## calc cell average values
xmin <- annual$hval[annual$hname == "xllcorner"]
ymin <- annual$hval[annual$hname == "yllcorner"]
nrow <- annual$hval[annual$hname == "nrows"]
ncol <- annual$hval[annual$hname == "ncols"]
res <- annual$hval[annual$hname == "cellsize"]
xscale <- seq(xmin,xmin+ncol*res,length=ncol)
yscale <- seq(ymin,ymin+nrow*res,length=nrow)
for(x in 1:nx){
  cols <- findInterval(c(xlim[1]+x-1,xlim[1]+x),xscale)
  for(y in 1:ny){
    rows <- findInterval(c(ylim[1]+y-1,ylim[1]+y),yscale)
    pbar[x,y] <- mean(annual$grid[cols[1]:cols[2],rows[1]:rows[2]])
    pdiff[cols[1]:cols[2],rows[1]:rows[2]] <- annual$grid[cols[1]:cols[2],rows[1]:rows[2]] - pbar[x,y]
  }
}

plot(dem$grid,pdiff,pch='.',xlim=c(0,2040))
#lines(lowess(pdiff ~ dem$grid),col=2,lwd=2)                    
#fit1 <- lm(as.vector(pdiff) ~ as.vector(dem$grid))
#fit1 <- slm(as.vector(dem$grid),as.vector(pdiff))
x <- as.vector(dem$grid)
y <- as.vector(pdiff)


### TOO SLOW, need to look at ways to pare down the data sets and/or look into external memory algorithms in R

  valid <- apply(cbind(is.na(x),is.na(y)),1,sum) == 0

  x <- x[valid]
  y <- y[valid]
  
  xbar <- mean(x)
  ybar <- mean(y)
  ssx <- sum((x-xbar)^2)
  spxy <- sum((x-xbar)*(y-ybar))
  b1 <- spxy/ssx
  b0 <- ybar-b1*xbar



#####################################################################
####
####            EXTERNAL MEMORY APPROACH

`read.grid.header` <- function(con){  
  rawheader <- readLines(con,6)
  header <- list()
  for(i in 1:6){
    h <- strsplit(rawheader[i]," ")[[1]]
    header[[i]] <- as.numeric(h[2])
    names(header)[i] <- h[1]
  }
  return(header)
}


## initialize
nline <- 10  ## number of lines to read at once
dem.fname <- "us_30s_dem.asc"
ppt.fname <- "us_ppt_1971_2000.14"
dem <- file(dem.fname,'r')
ppt <- file(ppt.fname,'r')
dem.head <- read.grid.header(dem)  ## read ascii grid headers
ppt.head <- read.grid.header(ppt)

## define course grid
lat <- seq(dem.head$yllcorner,by=dem.head$cellsize,length=dem.head$nrows)
lon <- seq(dem.head$xllcorner,by=dem.head$cellsize,length=dem.head$ncols)
xlim <- range(lon)
xlim[1] <- floor(xlim[1])
xlim[2] <- ceiling(xlim[2])
ylim <- range(lat)
ylim[1] <- floor(ylim[1])
ylim[2] <- ceiling(ylim[2])
nx <- xlim[2]-xlim[1]
ny <- ylim[2]-ylim[1]
ssx <- spxy <- xbar <- ybar <- n <- matrix(0,ny,nx)
xcell <- floor(lon)-xlim[1]+1


##first pass, calculate means
for(i in 1:dem.head$nrows){

  ## define row
  ycell <- floor(lat[i])-ylim[1] + 1
  ##read data
  x <- as.numeric(strsplit(readLines(dem,1)," ")[[1]])
  y <- as.numeric(strsplit(readLines(ppt,1)," ")[[1]])
  ##set NA's
  x[x == dem.head$NODATA_value] <- NA
  y[y == ppt.head$NODATA_value] <- NA
  valid <-  apply(cbind(is.na(x),is.na(y)),1,sum) 
  x[valid > 0] <- NA
  y[valid > 0] <- NA
  ## calc values
  ntmp <- tapply(x*0+1,xcell,sum,na.rm=TRUE)
  xtmp <- tapply(x,xcell,sum,na.rm=TRUE)
  ytmp <- tapply(y,xcell,sum,na.rm=TRUE)
  n[ycell,as.numeric(names(ntmp))]    <- n[ycell,as.numeric(names(ntmp))]   +ntmp
  xbar[ycell,as.numeric(names(xtmp))] <- xbar[ycell,as.numeric(names(xtmp))]+xtmp
  ybar[ycell,as.numeric(names(ytmp))] <- ybar[ycell,as.numeric(names(ytmp))]+ytmp 
}
close(dem)
close(ppt)
xbar <- xbar/n
ybar <- ybar/n

##second pass, calculate second moments
dem <- file(dem.fname,'r')
ppt <- file(ppt.fname,'r')
dem.head <- read.grid.header(dem)  ## read ascii grid headers
ppt.head <- read.grid.header(ppt)
for(i in 1:dem.head$nrows){

  ## define row
  ycell <- floor(lat[i])-ylim[1] + 1
  ##read data
  x <- as.numeric(strsplit(readLines(dem,1)," ")[[1]])
  y <- as.numeric(strsplit(readLines(ppt,1)," ")[[1]])
  ##set NA's
  x[x == dem.head$NODATA_value] <- NA
  y[y == ppt.head$NODATA_value] <- NA
  valid <-  apply(cbind(is.na(x),is.na(y)),1,sum) 
  x[valid > 0] <- NA
  y[valid > 0] <- NA
  ## calc values
  ssxtemp <- tapply((x-xbar[ycell,xcell])^2,xcell,sum,na.rm=TRUE)
  spxytemp <- tapply((x-xbar[ycell,xcell])*(y-ybar[ycell,xcell]),xcell,sum,na.rm=TRUE)
  ssx[ycell,as.numeric(names(ssxtemp))] <- ssx[ycell,as.numeric(names(ssxtemp))]+ssxtemp
  spxy[ycell,as.numeric(names(spxytemp))] <- spxy[ycell,as.numeric(names(spxytemp))]+spxytemp 
}
close(dem)
close(ppt)
b1 <- spxy/ssx
b0 <- ybar-b1*xbar
ybar[is.nan(ybar)] <- NA

save(n,xbar,ybar,spxy,ssx,b1,b0,file="lapse.Rdata")

