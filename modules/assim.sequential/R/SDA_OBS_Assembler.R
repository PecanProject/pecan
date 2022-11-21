#' Assembler for preparing obs.mean and obs.cov for the SDA workflow
#'
#' @param settings_dir the path of settings.xml object.
#' @param Var Variable name, currently support: SMP, AGB, and LAI.
#' @param OutDir the path to store obs.mean and obs.cov
#' @param Obs_Prep if your settings object doesn't contain Obs_Prep, you can import it separately (details see L17-18).
#' @param skip_buffer flag to skip calculating min var based on buffer area for agb data.
#'
#' @return list of obs.mean and obs.cov
#' @export
#' @author Dongchen Zhang
#'
#' @examples
#' settings_dir <- "/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA/pecan.xml"
#' OutDir <- "/projectnb/dietzelab/dongchen/All_NEON_SDA/test_OBS"
#' Var <- c("SMP", "LAI", "AGB")
#' OBS <- PEcAnAssimSequential::SDA_OBS_Assembler(settings_dir, Var, OutDir)

SDA_OBS_Assembler <- function(settings_dir, Var, OutDir, Obs_Prep = NULL, skip_buffer = TRUE){
  #read settings
  settings <- PEcAn.settings::read.settings(settings_dir)
  
  # I prefer to include Obs_Prep info in the settings object, that will make life easier!
  # if you don't like it, you can provide the Obs_Prep object 
  # followed strictly by L76 - L82 in "/pecan/modules/assim.sequential/inst/MultiSite-Exs/SDA/Create_Multi_settings.R"
  if(!is.null(settings$state.data.assimilation$Obs_Prep)){
    Obs_Prep <- settings$state.data.assimilation$Obs_Prep
  }else if(is.null(Obs_Prep)){
    PEcAn.logger::logger.info("Please provide info of Obs_Prep object!")
    return(0)
  }
  
  #prepare site_info offline, because we need to submit this to GEO, which doesn't support Bety connection.
  Site_Info <- list(site_id = settings %>% map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character(),
                    lat = settings %>% map(~.x[['run']] ) %>% map('site') %>% map('lat') %>% unlist() %>% as.numeric(),
                    lon = settings %>% map(~.x[['run']] ) %>% map('site') %>% map('lon') %>% unlist() %>% as.numeric(),
                    site_name = rep("name", length(settings %>% map(~.x[['run']] ) %>% map('site') %>% map('lat') %>% unlist() %>% as.numeric())))
  
  #We need to keep the order from Var to the actual obs.mean and obs.cov
  OBS <- list()
  new_var <- c()
  # for(i in 1:length(Var)){
  #   library(PEcAn.data.remote)
  #   fcn <- paste0(Var[i], "Prep")
  #   Temp_out <- do.call(fcn, args = list(
  #     
  #   ))
  # }
  for (i in 1:length(Var)) {
    var <- Var[i]
    if("AGB" == var){
      AGB_Output <- PEcAn.data.remote::AGB_prep(Site_Info = Site_Info, 
                                                Start_Date = Obs_Prep$Start_Date, 
                                                End_Date = Obs_Prep$End_Date,
                                                Time_Step = Obs_Prep$AGB$Time_Step,
                                                AGB_dir = Obs_Prep$AGB$AGB_dir,
                                                OutDir = Obs_Prep$AGB$Out_dir,
                                                Export_CSV = Obs_Prep$AGB$Export_CSV,
                                                Allow_download = Obs_Prep$AGB$Allow_download,
                                                buffer = as.numeric(Obs_Prep$AGB$buffer),
                                                skip_buffer = skip_buffer)
      time_points <- AGB_Output$time_points
      OBS[[i]] <- AGB_Output$AGB_Output
      new_var <- c(new_var, AGB_Output$var)
    }else if("LAI" == var){
      LAI_Output <- PEcAn.data.remote::LAI_prep(Site_Info = Site_Info, 
                                                Start_Date = Obs_Prep$Start_Date, 
                                                End_Date = Obs_Prep$End_Date,
                                                Time_Step = Obs_Prep$LAI$Time_Step,
                                                NCore = Obs_Prep$LAI$NCore,
                                                OutDir = Obs_Prep$LAI$Out_dir,
                                                Search_Window = Obs_Prep$LAI$Search_Window,
                                                Export_CSV = Obs_Prep$LAI$Export_CSV)
      time_points <- LAI_Output$time_points
      OBS[[i]] <- LAI_Output$LAI_Output
      new_var <- c(new_var, LAI_Output$var)
    }else if("SMP" == var){
      SMP_Output <- PEcAn.data.remote::SMP_prep(Site_Info = Site_Info, 
                                                  Start_Date = Obs_Prep$Start_Date, 
                                                  End_Date = Obs_Prep$End_Date,
                                                  Time_Step = Obs_Prep$SMAP$Time_Step,
                                                  OutDir = Obs_Prep$SMAP$Out_dir,
                                                  Search_Window = Obs_Prep$SMAP$Search_Window,
                                                  Export_CSV = Obs_Prep$SMAP$Export_CSV,
                                                  Update_CSV = Obs_Prep$SMAP$Update_CSV)
      time_points <- SMP_Output$time_points
      OBS[[i]] <- SMP_Output$SMP_Output
      new_var <- c(new_var, SMP_Output$var)
    }
  }
  
  #Create obs.mean and obs.cov
  obs.mean <- obs.cov <- list()
  new_diag <- function(vec){
    if(length(vec)==1){
      return(vec)
    }else{
      return(diag(vec))
    }
  }
  rm_na_diag <- function(mat){
    mat[-which(is.na(rowSums(mat))), -which(is.na(colSums(mat)))]
  }
  
  #over time
  for (i in 1:length(time_points)) {
    t <- time_points[i]
    dat_all_var <- sd_all_var <- matrix(NA, length(Site_Info$site_id), length(new_var)) %>% `colnames<-`(new_var)
    #over variable
    for (j in 1:length(OBS)) {
      if(paste0(t, "_", Var[j]) %in% colnames(OBS[[j]])){
        dat_all_var[,j] <- OBS[[j]][,paste0(t, "_", Var[j])]
        sd_all_var[,j] <- OBS[[j]][,paste0(t, "_SD")]^2 #convert from SD to Var
      }else{
        dat_all_var[,j] <- NA
        sd_all_var[,j] <- NA
      }
    }
    #over site
    site_dat_var <- site_sd_var <- list()
    for (j in 1:dim(dat_all_var)[1]) {
      site_dat_var[[j]] <- dat_all_var[j,] %>% matrix(1,length(new_var)) %>% data.frame %>% `colnames<-`(new_var)
      site_sd_var[[j]] <- new_diag(sd_all_var[j,])
    }
    obs.mean[[i]] <- site_dat_var %>% set_names(Site_Info$site_id)
    obs.cov[[i]] <- site_sd_var %>% set_names(Site_Info$site_id)
  }
  names(obs.mean) <- gsub("-", "/", time_points)#not sure if I have to do this.
  names(obs.cov) <- gsub("-", "/", time_points)
  
  #remove NA data as this will crash the SDA. Removes rown numbers (may not be nessesary)
  for (i in 1:length(obs.mean)) {
    for (j in 1:length(obs.mean[[i]])) {
      if(sum(is.na(obs.mean[[i]][[j]]))){
        obs.mean[[i]][[j]] <- obs.mean[[i]][[j]][-which(is.na(obs.mean[[i]][[j]]))]
        obs.cov[[i]][[j]] <- obs.cov[[i]][[j]][-which(is.na(rowSums(obs.cov[[i]][[j]]))), -which(is.na(colSums(obs.cov[[i]][[j]])))]
      }
    }
  }
  save(obs.mean, file = file.path(OutDir, "obs.mean.Rdata"))
  save(obs.cov, file = file.path(OutDir, "obs.cov.Rdata"))
  list(obs.mean = obs.mean, obs.cov = obs.cov)
}