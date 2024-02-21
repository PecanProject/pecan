##' @title hop_test
##' @name  hop_test
##' @author Ann Raiho \email{araiho@@nd.edu}
##' 
##' @param settings    SDA PEcAn settings object
##' @param nyear       number of years to run hop test over
##'
##' @description Hop test. This script tests that the model successfully reads it's own restart and can restart without loss of information.
##' 
##' @return NONE
##' @export
##' 
hop_test <- function(settings, ens.runid = NULL, nyear){
  ##### 
  ##### Variable to check
  ##### 
  hop_var <- lapply(settings$state.data.assimilation$state.variables,'[[','variable.name')
  
  ##### 
  ##### Regular Run
  ##### 
  if(is.null(ens.runid)){
    PEcAn.workflow::run.write.configs(settings, write = settings$database$bety$write)
    
    PEcAn.workflow::start_model_runs(settings, settings$database$bety$write)
    
    ens.runid <- utils::read.table(file.path(settings$rundir,'runs.txt'))
  }
  if (!requireNamespace("PEcAn.utils", quietly = TRUE)) {
    PEcAn.logger::logger.error(
      "Can't find package 'PEcAn.utils',",
      "needed by `PEcAnAssimSequential::hop_test()`.",
      "Please install it and try again.")
  }
  ens <- PEcAn.utils::read.output(runid = ens.runid, 
                     outdir = file.path(settings$outdir,'out', ens.runid), 
                     start.year = lubridate::year(settings$run$start.date), 
                     end.year = lubridate::year(settings$run$end.date), 
                     variables = hop_var)
  ##### 
  ##### Create Empty Data
  ##### 
  reg_run_end <- lubridate::year(settings$run$end.date)
  settings$run$end.date <- paste0(reg_run_end - (nyear-1),'/12/31') #changing the run end.date so that the .nc files get written after write_restart
  
  obs.mean <- obs.cov <- rep(list(NA),nyear)
  names(obs.mean) <- names(obs.cov) <- paste0(lubridate::year(settings$run$end.date):reg_run_end,'/12/31')
  
  ##### 
  ##### Run in SDA code with no data -- HOP Run
  ##### 
  PEcAnAssimSequential::sda.enkf(settings = settings, obs.mean = obs.mean, obs.cov = obs.cov)
  
  hop.runid <- utils::read.table(file.path(settings$rundir,'runs.txt'))
  hop.ens <- PEcAn.utils::read.output(runid = hop.runid, 
                     outdir = file.path(settings$outdir,'out', hop.runid), 
                     start.year = lubridate::year(settings$run$start.date), 
                     end.year = reg_run_end, 
                     variables = hop_var)
  
  save(list = ls(envir = environment(), all.names = TRUE), 
       file = file.path(settings$outdir, "hop_test_output.Rdata"), envir = environment())
  
  ##### 
  ##### Comparison: Hop Run versus Regular Run
  ##### 
  
  plot_years <- lubridate::year(settings$run$start.date):reg_run_end
  
  grDevices::pdf('hop_test_results.pdf')
  graphics::par(mfrow=c(2,1))
  for(p in seq_along(hop_var)){
    
    hop_var_use <- unlist(hop_var[p])
    ens.plot <- ens[[hop_var_use]][(length(plot_years)-nyear):length(plot_years)]
    hop.ens.plot <- hop.ens[[hop_var_use]][(length(plot_years)-nyear):length(plot_years)]
    
    plot(plot_years,
         ens[[hop_var_use]],
         pch=19,ylim=c(range(ens,hop.ens)),
         ylab = hop_var_use, xlab = 'Years')
    graphics::points(plot_years,
           hop.ens[[hop_var_use]],col='red')
    graphics::abline(v=year(settings$run$end.date),col='blue',lwd=2)
    graphics::legend('topleft', c('Regular Run','Hop Run','Test Start'), pch=c(19,1,19),col=c('black','red','blue'))
    graphics::title(paste('Hop Test Comparision',hop_var[p]))
    
    hop_cor <- stats::cor(ens.plot,hop.ens.plot)

    plot(ens.plot,hop.ens.plot,
         xlab = paste('Regular Run',hop_var_use),
         ylab = paste('Hop Run',hop_var_use),pch=19,cex=1.5)
    
    graphics::abline(a=0,b=1,col='red',lwd=2)
    graphics::legend('topleft',paste('Correlation =',hop_cor))
    
    graphics::title(paste('Hop Test Correlation',hop_var[p]))
  }
  grDevices::dev.off()
  
}




