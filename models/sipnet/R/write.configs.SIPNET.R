#--------------------------------------------------------------------------------------------------#
# Template for functions to prepare and write out files model-specific configuration files for MA
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Writes a configuration files for your model
#--------------------------------------------------------------------------------------------------#
write.config.SIPNET <- function(defaults, trait.values, settings, outdir, run.id){

  ### WRITE sipnet.in
  template <- system.file("sipnet.in", package="PEcAn.SIPNET")
  config.text <- readLines(con=template, n=-1)
  config.text <- gsub('@FILENAME@', run.id, config.text)
  config.file.name <- paste(run.id,".in", sep='')
  writeLines(config.text, con = paste(outdir, config.file.name, sep=''))
    
  ### Display info to the console.
  print(run.id)

  
  ### WRITE *.param
  
  return()

  
  startdate <- as.Date(settings$run$start.date)
  enddate <- as.Date(settings$run$end.date)
  
  #-----------------------------------------------------------------------
  ### Edit a templated config file for runs
  
  config.text <- gsub('@SITE_LAT@', settings$run$site$lat, config.text)
  config.text <- gsub('@SITE_LON@', settings$run$site$lon, config.text)
  config.text <- gsub('@SITE_MET@', settings$run$site$met, config.text)
  config.text <- gsub('@MET_START@', settings$run$site$met.start, config.text)
  config.text <- gsub('@MET_END@', settings$run$site$met.end, config.text)

    #-----------------------------------------------------------------------
    config.text <- gsub('@START_MONTH@', format(startdate, "%m"), config.text)
    config.text <- gsub('@START_DAY@', format(startdate, "%d"), config.text)
    config.text <- gsub('@START_YEAR@', format(startdate, "%Y"), config.text)
    config.text <- gsub('@END_MONTH@', format(enddate, "%m"), config.text)
    config.text <- gsub('@END_DAY@', format(enddate, "%d"), config.text)
    config.text <- gsub('@END_YEAR@', format(enddate, "%Y"), config.text)

    #-----------------------------------------------------------------------
    config.text <- gsub('@OUTDIR@', settings$run$host$outdir, config.text)
    config.text <- gsub('@ENSNAME@', run.id, config.text)

  
    ### Generate a numbered suffix for scratch output folder.  Useful for cleanup.  TEMP CODE. NEED TO UPDATE.
    #cnt = counter(cnt) # generate sequential scratch output directory names 
    #print(cnt)
    #scratch = paste(Sys.getenv("USER"),".",cnt,"/",sep="")
    scratch = Sys.getenv("USER")
    #config.text <- gsub('@SCRATCH@', paste('/scratch/', settings$run$scratch, sep=''), config.text)
    config.text <- gsub('@SCRATCH@', paste('/scratch/', scratch, sep=''), config.text)
    ###
  
    config.text <- gsub('@OUTFILE@', paste('out', run.id, sep=''), config.text)
 
    #-----------------------------------------------------------------------

}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' @name write.run.generic
##' @title Function to generate generic model run script files
##' @author <unknown>
##' @import PEcAn.utils
#--------------------------------------------------------------------------------------------------#
write.run.generic <- function(settings){
  run.script.template = system.file("data", "run.template.generic", package="PEcAn.generic")
  run.text <- scan(file = run.script.template, 
                   what="character",sep='@', quote=NULL, quiet=TRUE)
  run.text  <- gsub('TMP', paste("/scratch/",Sys.getenv("USER"),sep=""), run.text)
  run.text  <- gsub('BINARY', settings$run$host$ed$binary, run.text)
  run.text <- gsub('OUTDIR', settings$run$host$outdir, run.text)
  runfile <- paste(settings$outdir, 'run', sep='')
  writeLines(run.text, con = runfile)
  if(settings$run$host$name == 'localhost') {
    system(paste('cp ', runfile, settings$run$host$rundir))
  }else{
    system(paste("rsync -outi ", runfile , ' ', settings$run$host$name, ":",
                 settings$run$host$rundir, sep = ''))
  }
}
#==================================================================================================#



####################################################################################################
### EOF.  End of R script file.            	
####################################################################################################
