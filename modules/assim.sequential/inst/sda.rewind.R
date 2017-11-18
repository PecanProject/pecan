##' @title sda.rewind
##' @name  sda.rewind
##' @author Ann Raiho \email{dietze@@bu.edu}
##' 
##' @param settings          SDA settings object
##' @param run.id            list of current run.ids
##' @param time_to_rewind    year that should be deleted last
##' 
##' @description Helper function for deleting SDA files to be able to run from a specified point
##' 
##' @return NA
##' @export
##' 

sda_rewind <- function(settings,run.id,time_to_rewind){
  for(i in 1:length(run.id)){
    file.rename(paste0(settings$outdir,
                       '/out/',run.id[[i]],
                       '/',time_to_rewind,'-01-01linkages.out.Rdata'),
                paste0(settings$outdir,
                       '/out/',run.id[[i]],
                       '/linkages.out.Rdata'))
    for(t in time_to_rewind:year(settings$state.data.assimilation$end.date)){
      file.remove(paste0(settings$outdir,
                         '/out/',run.id[[i]],
                         '/',t,'.nc'))
      file.remove(paste0(settings$outdir,
                         '/out/',run.id[[i]],
                         '/',t,'.nc.var'))
    }
  }
}

sda_rewind(settings, run.id, time_to_rewind = 1962)

