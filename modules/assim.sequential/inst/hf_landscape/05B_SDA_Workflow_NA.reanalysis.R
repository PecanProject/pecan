## Reanalysis helper script around 05_SDA_Workflow_NA

runDays <- seq(as.Date("2022-05-20"), as.Date("2023-04-12"), by="days")
FORCE = FALSE  ## should we overwrite previously completed runs

for (s in seq_along(runDays)) {
  ## did we do this run already?
  now  = paste0("/projectnb/dietzelab/dietze/hf_landscape_SDA/test01/FNA",runDays[s])
  print(now)
  this.out = dir(file.path(now,"out"),full.names = TRUE)
  if(length(this.out) > 0 & !FORCE) break
  
  ## find previous run
  yesterday = runDays[s] - lubridate::days(1)
  prev = paste0("/projectnb/dietzelab/dietze/hf_landscape_SDA/test01/FNA",yesterday)
  if(dir.exists(prev)){
    ## is there output there?
    prev.out = dir(file.path(prev,"out"),full.names = TRUE)
    if(length(prev.out)>0){
      prev.files = sapply(as.list(prev.out),function(x){length(dir(x,pattern = "*.nc"))})
      if(min(prev.files)>0){
        
        #########   RUN FORECAST   ########
        msg = system2("/home/dietze/pecan/modules/assim.sequential/inst/hf_landscape/05_SDA_Workflow_NA.R",
                      paste("--start.date",runDays[s],"--prev",prev),
                      wait=TRUE,
                      stdout="stdout.log",
                      stderr="stderr.log")
        print(msg)
        
      }
    } else { break }
  } else {
    ## previous run didn't occur
    break
  }
  
}