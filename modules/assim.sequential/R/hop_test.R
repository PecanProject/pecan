##' @title hop_test
##' @name  hop_test
##' @author Ann Raiho \email{araiho@@nd.edu}
##' 
##' @param settings    SDA PEcAn settings object
##'
##' @description Hop test. This script tests that the model successfully reads it's own restart and can restart without loss of information.
##' 
##' @return NONE
##' @export
##' 
hop_test <- function(settings){
  ##### 
  ##### Variable to check
  ##### 
  hop_var <- settings$state.data.assimilation$state.variables$variable$variable.name
  
  ##### 
  ##### Regular Run
  ##### 
  settings <- run.write.configs(settings, write = settings$database$bety$write, ens.sample.method = "halton")
  
  PEcAn.remote::start.model.runs(settings, settings$database$bety$write)
  
  ens.runid <- read.table(file.path(settings$rundir,'runs.txt'))
  ens <- read.output(runid = ens.runid, 
                     outdir = file.path(settings$outdir,'out', ens.runid), 
                     start.year = lubridate::year(settings$run$start.date), 
                     end.year = lubridate::year(settings$run$end.date), 
                     variables = hop_var)
  ##### 
  ##### Create Empty Data
  ##### 
  reg_run_end <- lubridate::year(settings$run$end.date)
  settings$run$end.date <- paste0(reg_run_end - 9,'/12/31') #changing the run end.date so that the .nc files get written after write_restart
  
  obs.mean <- obs.cov <- rep(list(NA),10)
  names(obs.mean) <- names(obs.cov) <- paste0(lubridate::year(settings$run$end.date):reg_run_end,'/12/31')
  
  ##### 
  ##### Run in SDA code with no data -- HOP Run
  ##### 
  sda.enkf(settings = settings, obs.mean = obs.mean, obs.cov = obs.cov)
  
  hop.runid <- read.table(file.path(settings$rundir,'runs.txt'))
  hop.ens <- read.output(runid = hop.runid, 
                     outdir = file.path(settings$outdir,'out', hop.runid), 
                     start.year = lubridate::year(settings$run$start.date), 
                     end.year = reg_run_end, 
                     variables = hop_var)
  
  ##### 
  ##### Comparison: Hop Run versus Regular Run
  ##### 
  hop_cor <- cor(ens[[hop_var]],hop.ens[[hop_var]])
  
  pdf('hop_test_results.pdf')
  plot(ens[[hop_var]],hop.ens[[hop_var]],
       ylab = paste('Regular Run',hop_var),
       xlab = paste('Hop Run',hop_var),pch=19,cex=1.5)
  
  abline(a=0,b=1,col='red',lwd=2)
  legend('topleft',paste('Correlation =',hop_cor))
  
  title('Hop Test Results')
  dev.off()
  
}




