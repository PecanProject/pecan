#--------------------------------------------------------------------------------------------------#
##'
##' @name model2netcdf.SIPNET
##' @title Function to convert SIPNET model output to standard netCDF format
##' @param outdir Location of SIPNET model output
##' @param run.id Name of SIPNET model output file.
##' 
##' @export
##' @author Shawn Serbin, Michael Dietze
#--------------------------------------------------------------------------------------------------#

model2netcdf.SIPNET <- function(outdir,run.id) {
  
  require(ncdf)
  
  ### Read in model output in SIPNET format
  sipnet.output <- read.table(paste(outdir,run.id,".out",sep=""),header=T,skip=1,sep='')
  sipnet.output.dims <- dim(sipnet.output)
  
  ### Determine number of years and output timestep
  num.years <- length(unique(sipnet.output$year))
  years <- unique(sipnet.output$year)
  timestep.s <- 86400/length(which(sipnet.output$year==years[1] & sipnet.output$day==1))
  
  ### Loop over years in SIPNET output to create separate netCDF outputs
  for (y in years){
    print(y)
    
    ### Setup outputs for netCDF file in appropriate units
    output <- list()
    output[[1]] <- sipnet.output$year
    output[[2]] <- (sipnet.output$gpp*0.001)/timestep.s     # GPP in kgC/m2/s
    output[[3]] <- (sipnet.output$npp*0.001)/timestep.s     # NPP in kgC/m2/s
    output[[4]] <- (sipnet.output$rtot*0.001)/timestep.s    # Total Respiration in kgC/m2/s
    output[[5]] <- (sipnet.output$rAboveground*0.001)/timestep.s +
      (sipnet.output$rRoot*0.001)/timestep.s                # Autotrophic Respiration in kgC/m2/s
    output[[6]] <- (sipnet.output$rSoil*0.001)/timestep.s   # Heterotropic Respiration in kgC/m2/s
    output[[7]] <- (sipnet.output$nee*0.001)/timestep.s     # NEE in kgC/m2/s

    #******************** Declar netCDF variables ********************#
    t <- dim.def.ncdf("time","seconds",(1:sipnet.output.dims[1]*timestep.s)) #cumulative time
    
    var <- list()
    var[[1]]  <- var.def.ncdf("Year","YYYY",t,-999)     # year
    var[[2]]  <- var.def.ncdf("GPP","kgC/m2/s",t,-999)  # GPP in kgC/m2/s
    var[[3]]  <- var.def.ncdf("NPP","kgC/m2/s",t,-999)  # NPP in kgC/m2/s
    var[[4]]  <- var.def.ncdf("TotalResp","kgC/m2/s",t,-999)  # Total Respiration in kgC/m2/s
    var[[5]]  <- var.def.ncdf("AutoResp","kgC/m2/s",t,-999)  # Autotrophic respiration in kgC/m2/s
    var[[6]]  <- var.def.ncdf("HeteroResp","kgC/m2/s",t,-999)  # Heterotrophic respiration in kgC/m2/s
    var[[7]]  <- var.def.ncdf("NEE","kgC/m2/s",t,-999)  # NEE in kgC/m2/s
    
    #******************** Declar netCDF variables ********************#
    nc <- create.ncdf(paste(outdir,run.id,y,".nc",sep=""),var)
    
    ### Output netCDF data
    for(i in 1:length(var)){
      print(i)
      put.var.ncdf(nc,var[[i]],output[[i]])  
    }
    close.ncdf(nc)
    
  } ### End of year loop

} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################