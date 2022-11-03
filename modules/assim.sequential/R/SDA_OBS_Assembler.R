SDA_OBS_Assembler <- function(settings_dir, Start_Date, End_Date, Var, Search_Window, Outdir){
  settings <- PEcAn.settings::read.settings(settings_dir)
  
  if("AGB" %in% Var){
    
  }
  if("LAI" %in% Var){
    
  }
  if("SMAP" %in% Var){
    
  }
  
}

test <- settings$state.data.assimilation$Obs_Prep

settings %>% map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character()