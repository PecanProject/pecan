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
      
      ### Create file to keep old output in.
      dir_put <- file.path(settings$outdir,'out',run.id[[i]],as.numeric(time_to_rewind)-1)
      if(!dir.exists(dir_put)) dir.create(dir_put)
      
      if(file.exists(file.path(settings$outdir,'out',run.id[[i]],'linkages.out.Rdata'))){
        file.copy(from = file.path(settings$outdir,'out',run.id[[i]],'linkages.out.Rdata'),to = dir_put) #going to be a lot of memory. might want to just take model specific files
      }else{
        print(paste('No linkages.out.Rdata for', run.id[[i]]))
      }
      
      ### Rename linkages specific files
      if(file.exists(paste0(settings$outdir,
                            '/out/',run.id[[i]],
                            '/',time_to_rewind,'-12-31 23:59:59linkages.out.Rdata'))){
        
        file.rename(paste0(settings$outdir,
                           '/out/',run.id[[i]],
                           '/',time_to_rewind,'-12-31 23:59:59linkages.out.Rdata'),
                    paste0(settings$outdir,
                           '/out/',run.id[[i]],
                           '/linkages.out.Rdata'))
        
      }
      
      
      ### Remove netcdfs
      for(t in (as.numeric(time_to_rewind)+1):year(settings$state.data.assimilation$end.date)){
        
        file.remove(paste0(settings$outdir,
                           '/out/',run.id[[i]],
                           '/',t,'.nc'))
        
        file.remove(paste0(settings$outdir,
                           '/out/',run.id[[i]],
                           '/',t,'.nc.var'))
      }
      print(paste('editing runid',run.id[[i]]))
    }
  }
  
  #--- Updating the nt and etc
  if(!dir.exists(file.path(settings$outdir,"SDA",as.numeric(time_to_rewind)-1))) dir.create(file.path(settings$outdir,"SDA",as.numeric(time_to_rewind)-1))
  
  # finding/moving files to it's end year dir
  files.last.sda <- list.files.nodir(file.path(settings$outdir,"SDA"))
  
  #copying
  if(file.exists(file.path(settings$outdir,"SDA"))){
    file.copy(file.path(file.path(settings$outdir,"SDA"),files.last.sda),
              file.path(file.path(settings$outdir,"SDA"),paste0(as.numeric(time_to_rewind)-1,"/",files.last.sda)))
    
    load(file.path(settings$outdir,"SDA",'sda.output.Rdata'))
    
    X <- FORECAST[[t]]
    FORECAST[t] <- NULL
    ANALYSIS[t] <- NULL
    enkf.params[t] <- NULL
    
    for(i in 1:length(new.state)) new.state[[i]] <- ANALYSIS[[t-1]][,i] #not sure if this should be t or t-1
    
    t = t-1
    
    save(site.locs, t, X, FORECAST, ANALYSIS, enkf.params, new.state, new.params, run.id,
         ensemble.id, ensemble.samples, inputs, Viz.output,  file = file.path(settings$outdir,"SDA", "sda.output.Rdata"))
  }
  
  ### Paleon specific with leading zero dates
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

#sda_rewind(settings, run.id, time_to_rewind = as.character(951))

# for example if you want to restart your sda run 
# where t=1 and obs.times = 950 then you want time_to_rewind 
# to be 951 because that is the last year of model 
# run data you don't want


