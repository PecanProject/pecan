####################################################################################################
#
#
#
#
#
#
#  	--- Last updated:  XXX BY Shawn Serbin <sserbin@bnl.gov>
####################################################################################################


#--------------------------------------------------------------------------------------------------#
# Close all devices and delete all variables.
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
#### Load R libraries
library(ggplot2)
library(readr)
library(tidyverse)
library(ncdf4)
library(scales)
library(lubridate)
library(osfr)
library(dplyr)
library(PEcAn.utils)
library(bigleaf)  # for unit conversions
#umolCO2.to.gC(-9.374368e-09, constants = bigleaf.constants())  # gC m-2 d-1
library(RColorBrewer)
# display.brewer.all()

osfr::osf_auth("PUT_YOUR_OSF_KEY_HERE")
`%notin%` <- Negate(`%in%`)

## options
Var <- "GPP"
ensembles <- 20
year_select <- seq(2002,2014,1)
Site <- "2000002567" #"2000002575", 2000002574, 2000002570, 2000002583, 2000002567
Site_name <- "US-Me2"  #US-Syv, US-PFa, US-NR1, US-MMS, US-Me2
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Output figure directory
sda_plots_dir <- file.path(paste0('~/Data/Dropbox/MANUSCRIPTS/BNL_TEST/Dokoohaki_etal_NASA_CMS_SDA_part1/Figures/',
                                  Site_name))
if (! file.exists(sda_plots_dir)) dir.create(file.path(sda_plots_dir),recursive=TRUE, showWarnings = FALSE)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Get FLUXNET data
nasa_cms <- osfr::osf_retrieve_node("https://osf.io/efcv5/")
nasa_cms
nasa_cms_dirs <- osf_ls_files(nasa_cms)
nasa_cms_dirs
fluxnet_dir <- osf_ls_files(nasa_cms, path = nasa_cms_dirs$name[4])
fluxnet_dir
fluxnet_dirs <- osf_ls_files(fluxnet_dir, n_max = Inf)
fluxnet_dirs

fluxnet_site_data_num <- grep(Site_name, fluxnet_dirs$name) 
fluxnet_site_data <- fluxnet_dirs[fluxnet_site_data_num,]

# INFO: https://fluxnet.fluxdata.org/data/fluxnet2015-dataset/subset-data-product/
fluxnet_data_dir <- file.path('~/Data/Dropbox/MANUSCRIPTS/BNL_TEST/Dokoohaki_etal_NASA_CMS_SDA_part1/Data/FLUXNET')
if (! file.exists(fluxnet_data_dir)) dir.create(file.path(fluxnet_data_dir),recursive=TRUE, showWarnings = FALSE)

# $links$download
# fluxnet_site_data$meta[[1]]$links$download # to get download link
fluxnet_local_file <- osf_retrieve_file(fluxnet_site_data$meta[[1]]$links$download) %>% 
  osf_download(conflicts = "skip", path = file.path(fluxnet_data_dir),
               verbose = TRUE, progress = TRUE)
fluxnet_data_dir_uncomp <- file.path(fluxnet_data_dir,
                                     gsub(".zip", "", basename(fluxnet_local_file$local_path)))
unzip(fluxnet_local_file$local_path, exdir = fluxnet_data_dir_uncomp, overwrite = FALSE)
local_flux_files <- list.files(gsub(pattern = ".zip", replacement = "", fluxnet_local_file$local_path))

# weekly data
fluxnet_ww_data <- read.csv(file = file.path(fluxnet_data_dir_uncomp,
                                             local_flux_files[grep("WW", local_flux_files)]), header = T)
head(fluxnet_ww_data)
fluxnet_ww_data[fluxnet_ww_data == -9999.00000] <- NA
range(fluxnet_ww_data$NEE_VUT_REF, na.rm=T)
weeks <- as.Date(as.character(fluxnet_ww_data$TIMESTAMP_END),format="%Y%m%d")
png(filename = file.path(fluxnet_data_dir,paste0("FLX_",Site_name,"_FLUXNET2015_SUBSET_WW_1999-2014_1-4.png")), 
    width = 2800, height = 1200, res = 200)
par(mfrow=c(1,1), mar=c(4.5,4.7,0.3,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
plot(weeks,fluxnet_ww_data$NEE_VUT_REF, type="l", ylab="NEE (gC m-2 d-1)", lwd=2)
box(lwd=2.2)
dev.off()

# monthly data
fluxnet_mm_data <- read.csv(file = file.path(fluxnet_data_dir_uncomp,
                                             local_flux_files[grep("_MM_", local_flux_files)]), header = T)
head(fluxnet_mm_data)
fluxnet_mm_data[fluxnet_mm_data == -9999.00000] <- NA
range(fluxnet_mm_data$NEE_VUT_REF, na.rm=T)
months <- as.Date(paste0(as.character(fluxnet_mm_data$TIMESTAMP), '01'), format = '%Y%m%d')
png(filename = file.path(fluxnet_data_dir,paste0("FLX_",Site_name,"_FLUXNET2015_SUBSET_MM_1999-2014_1-4.png")), 
    width = 2800, height = 1200, res = 200)
par(mfrow=c(1,1), mar=c(4.5,4.7,0.3,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
plot(months,fluxnet_mm_data$NEE_VUT_REF, type="l", ylab="NEE (gC m-2 d-1)", lwd=2)
box(lwd=2.2)
dev.off()
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Get SDA weights from OSF
nasa_cms <- osfr::osf_retrieve_node("https://osf.io/efcv5/")
nasa_cms
nasa_cms_dirs <- osf_ls_files(nasa_cms)
nasa_cms_dirs

sda_results_dir <- file.path('~/Data/Dropbox/MANUSCRIPTS/BNL_TEST/Dokoohaki_etal_NASA_CMS_SDA_part1/Data/SDA_results')
if (! file.exists(sda_results_dir)) dir.create(file.path(sda_results_dir),recursive=TRUE, showWarnings = FALSE)
site_level_w <- osf_retrieve_file("https://osf.io/wyd9a/") %>% 
  osf_download(conflicts = "skip", path = file.path(sda_results_dir))
site_level_w <- readRDS(site_level_w$local_path)
head(site_level_w)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Get SDA ncfiles from OSF
ncdf_data <- osf_ls_files(nasa_cms, path = nasa_cms_dirs$name[5])
ncdf_data
ncdf_file_num <- grep(Var, ncdf_data$name) 
ncdf_file <- ncdf_data[ncdf_file_num,]

ncdf_sda_local_file <- osf_retrieve_file(ncdf_file$meta[[1]]$links$download) %>% 
  osf_download(conflicts = "skip", path = file.path(sda_results_dir),
               verbose = TRUE, progress = TRUE)
ncdf_sda_ncin <- nc_open(ncdf_sda_local_file$local_path)
ncdf_sda_ncin
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
rw <- ncvar_get(ncdf_sda_ncin, 'weights_rrel')
sites <- ncvar_get(ncdf_sda_ncin, 'site') %>% as.character()
Year <- ncvar_get(ncdf_sda_ncin, 'Year')
time <-  ncvar_get(ncdf_sda_ncin, 'time')
tunits <- ncatt_get(ncdf_sda_ncin, "time", "units")
var.nc <- ncvar_get(ncdf_sda_ncin, Var)

# keep the focal site
keep <- which(sites %in% Site)

#convert time
data_posix_time <- seq.POSIXt(as.POSIXct("1986-01-01 00:00:00", tz="US/Eastern"),
                              as.POSIXct("2018-12-30 21:00:00", tz="US/Eastern"),
                              length.out = length(time))

png(filename = file.path(sda_plots_dir,paste0(Site_name,'_raw_SDA_',Var,'_kgC_m2_s1.png')), 
    width = 2800, height = 1200, res = 200)
par(mfrow=c(1,1), mar=c(4.5,4.7,0.3,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
plot(data_posix_time,var.nc[,keep,1], type="l", ylab=paste0(Var," (kgC m-2 s-1)"), lwd=2)
box(lwd=2.2)
dev.off()

### -- Reformating the nc from matrix to data.frame --- DRAFT!
# which site to extract?
flux.data <- keep %>% 
  map_dfr(function(site){
    seq_len(ensembles) %>% # foreach ensemble
      #seq_len(1) %>% # foreach ensemble
      map_dfr(~{
        var.nc[, keep, .x] %>%
          as.data.frame() %>%
          mutate(Site=sites[keep],
                 ensemble=.x
          ) %>%
          `colnames<-`(c(Var, 'Site', 'ensemble')) %>%
          mutate(posix_time=data_posix_time)
      })
  })
head(flux.data)

# convert to umols C m-2 s-1 from kgC m-2 s-1
temp_convert <- misc.convert(flux.data[,Var], "kg C m-2 s-1", "umol C m-2 s-1")
temp_convert.2 <- umolCO2.to.gC(temp_convert)  # gC m-2 3h-1
flux.data[,Var] <- temp_convert.2
head(flux.data)
rm(temp_convert, temp_convert.2)
#hist(flux.data[,Var])
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
keep <- which(site_level_w$Site %in% Site)
temp <- site_level_w[keep,]
head(temp)
site_level_w_1site <- temp

new.p <- flux.data %>%
  mutate(Year=lubridate::year(posix_time)) %>%
  left_join(site_level_w_1site , 
            by=c('Site'='Site', 
                 'ensemble'='ens',
                 'Year'='Year'))
head(new.p)

## currently SDA has 53 weeks and FLUXNET 52?  Seems we need to fix date def for SDA
unique(lubridate::week(new.p$posix_time))
flux.data.df <- new.p %>%
  mutate(Week=lubridate::week(posix_time)) %>%
  group_by(Site, Year, Week)
flux.data.df[flux.data.df$Week == 53, "Week"] <- 52 # make SDA match FLUXNET definition of weeks (total of 52)
flux.data.df$floor_date <- lubridate::floor_date(as.Date(flux.data.df$posix_time), unit="week")
flux.data.df$ceiling_date <- lubridate::ceiling_date(flux.data.df$posix_time, unit="week")
unique(flux.data.df$Week)
head(flux.data.df)

flux.data.df <- flux.data.df %>%
  group_by(Site, Year, Week) %>%
  summarise(
    Forecast = mean(get(Var)),
    ForecastVar = var(get(Var)),
    Analysis= Hmisc::wtd.mean(get(Var), weights = Relative_weight),
    AnalysisVar= Hmisc::wtd.var(get(Var), weights = Relative_weight, normwt=TRUE, method='unbiased'),
  )
head(flux.data.df)

flux.data.df <- flux.data.df %>%
  mutate(Date=as.Date(
    paste(Year, Week, 1), 
    format = "%Y %U %u",
    tz = "US/Eastern"
  )) 
head(flux.data.df)
unique(flux.data.df$Week)

flux.data.df <- flux.data.df %>%
  mutate(
    ForecastSD=sqrt(ForecastVar),
    AnalysisSD=sqrt(AnalysisVar)
  ) %>%
  mutate(
    FLL =Forecast + ForecastSD, # Change this to what level of uncertainty you wanna plot
    FUL =Forecast - ForecastSD,
    ALL =Analysis + AnalysisSD,
    AUL =Analysis - AnalysisSD
  )
names(flux.data.df)
head(flux.data.df)
unique(flux.data.df$Week)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
flux.data.df_sub <- flux.data.df %>%
  filter(Year %in% year_select)
head(flux.data.df_sub)

names(fluxnet_ww_data)
fluxnet_ww_data_sub <- fluxnet_ww_data %>%
  mutate(Date=as.Date(paste0(as.character(TIMESTAMP_START), '01'), format='%Y%m%d')) %>%
  filter(lubridate::year(Date) %in% year_select) %>%
  mutate(Week = 1:n())
head(fluxnet_ww_data_sub)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Figures

# quick analysis/forecast fig, based on Hamze's example
flux.data.df %>%
  split(.$Site) %>%
  {.[1:1]} %>% # plot just the first site
  map(~ {
    .x %>%
      mutate(Date=as.POSIXct(Date))%>%
      ggplot(aes(x=Date))+
      geom_pointrange(aes(y= Forecast, ymin=FLL, ymax=FUL, color="Forecast"), alpha=0.75)+
      geom_pointrange(aes(y= Analysis, ymin=ALL, ymax=AUL, color="Analysis"), alpha=0.75)+
      labs(title = .x$Site, y=Var)+
      scale_color_manual(values=c("#ff7f00",'#4daf4a','#984ea3'), name="")+
      theme_minimal(base_size = 15)+
      scale_x_datetime(date_breaks = "2 year")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(legend.position = "top")
    ggsave(file=file.path(sda_plots_dir,paste0("timeseries_",.x$Site %>% unique(),".png")),
           width=14, height=7)
    #browser()
  })


# quick comparison
# for (i in seq_along(year_select)) {
#   print(year_select[i])
#   fluxnet_ww_data_sub_yr <- fluxnet_ww_data_sub %>%
#     filter(lubridate::year(Date) == year_select[i]) %>%
#     mutate(Week = 1:n())
#   flux.data.df_sub_yr <- flux.data.df_sub %>%
#     filter(Year %in% year_select[i])
#   
#   png(filename = file.path(sda_plots_dir,paste0(Site_name,'_SDA_',year_select[i],'_NEE_weeklty_comparison_v1.png')), 
#       width = 2800, height = 1200, res = 200)
#   par(mfrow=c(1,1), mar=c(4.5,4.7,0.3,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
#   plot(fluxnet_ww_data_sub_yr$Week,fluxnet_ww_data_sub_yr$NEE_VUT_REF, type="l", ylab="NEE (gC m-2 d-1)",
#        xlab = paste0("Week (",year_select[i],")"), ylim = c(-10,4))
#   lines(flux.data.df_sub_yr$Week, flux.data.df_sub_yr$Analysis, lty=2, lwd=2, col="grey40")
#   legend("bottomright", legend = c("FLUXNET", "SDA"),lty=c(1,2), cex=2)
#   box(lwd=2.2)
#   dev.off()
#   rm(fluxnet_ww_data_sub_yr,flux.data.df_sub_yr)
# }


#   Site        Year  Week Forecast ForecastVar Analysis AnalysisVar Date       ForecastSD AnalysisSD   FLL    FUL   ALL    AUL

head(fluxnet_ww_data_sub)
#fluxnet_ww_data_sub[,paste0(Var,"_NT_VUT_REF")]
if (Var %in% c("GPP","RECO")) {
  varef <- paste0(Var,"_NT_VUT_REF")
  ymin_var <- paste0(Var,"_NT_VUT_25")
  ymax_var <- paste0(Var,"_NT_VUT_75")
} else {
  varef <- paste0(Var,"_VUT_REF")
  ymin_var <- paste0(Var,"_VUT_25")
  ymax_var <- paste0(Var,"_VUT_75")
}

# brewer.pal(5, "Dark2")
for (i in seq_along(year_select)) {
  print(year_select[i])
  fluxnet_ww_data_sub_yr <- fluxnet_ww_data_sub %>%
    filter(lubridate::year(Date) == year_select[i]) %>%
    mutate(Week = 1:n())
  flux.data.df_sub_yr <- flux.data.df_sub %>%
    filter(Year %in% year_select[i])
  
  p <- ggplot() + 
    geom_ribbon(data=flux.data.df_sub_yr, aes(x=Week, y=Forecast, ymin=FLL, ymax=FUL),
                alpha=0.3, colour="black", fill = "#1B9E77") + theme_bw(base_size = 15) + 
    geom_point(data=flux.data.df_sub_yr, aes(x=Week, y=Forecast), colour="#1B9E77",
               alpha=0.85, size = 5, shape=18) + 
    geom_line(data=flux.data.df_sub_yr, aes(x=Week, y=Forecast),
                                                 colour="#1B9E77") +
    geom_ribbon(data=flux.data.df_sub_yr, aes(x=Week, y=Analysis, ymin=ALL, ymax=AUL),
                alpha=0.3, colour="black", fill = "#D95F02") + theme_bw(base_size = 15) +
    geom_point(data=flux.data.df_sub_yr, aes(x=Week, y=Analysis), colour="#D95F02",
               alpha=0.75, size = 5) + geom_line(data=flux.data.df_sub_yr, aes(x=Week, y=Analysis),
                                                 colour="#D95F02") + 
    geom_errorbar(data=fluxnet_ww_data_sub_yr, 
                         aes(x=Week, ymin=get(ymin_var), ymax=get(ymax_var))) + 
    geom_line(data=fluxnet_ww_data_sub_yr,aes(x=Week,y=get(varef))) + 
    ylab(bquote(.(Var) ~ (gC~m^{-2}~d^{-1}))) + 
    xlab(paste0("Week ",year_select[i]))
  ggsave(p,file=file.path(sda_plots_dir,paste0(Site_name,"_SDA_",year_select[i],"_",Var,"_weekly_comparison_v2.png")),
         width=14, height=7)
  rm(p,fluxnet_ww_data_sub_yr,flux.data.df_sub_yr)
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### EOF