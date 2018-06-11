##' @title sda.rewind
##' @name  sda.rewind
##' @author Ann Raiho \email{araiho@nd.edu}
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
  if(nchar(time_to_rewind) == 4){
    for(i in 1:length(run.id)){
      file.rename(paste0(settings$outdir,
                         '/out/',run.id[[i]],
                         '/',time_to_rewind,'-12-31 23:59:59linkages.out.Rdata'),
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

  if(nchar(time_to_rewind) == 3){
    for(i in 1:length(run.id)){
      file.rename(paste0(settings$outdir,
                         '/out/',run.id[[i]],
                         '/',time_to_rewind,'-12-31 23:59:59linkages.out.Rdata'),
                  paste0(settings$outdir,
                         '/out/',run.id[[i]],
                         '/linkages.out.Rdata'))
      for(t in time_to_rewind:year(settings$state.data.assimilation$end.date)){
        file.remove(paste0(settings$outdir,
                           '/out/',run.id[[i]],
                           '/','0',t,'.nc'))
        file.remove(paste0(settings$outdir,
                           '/out/',run.id[[i]],
                           '/','0',t,'.nc.var'))
      }
    }
  }
}

sda_rewind(settings, run.id, time_to_rewind = as.character(951))

# for example if you want to restart your sda run 
# where t=1 and obs.times = 950 then you want time_to_rewind 
# to be 951 because that is the last year of model 
# run data you don't want


