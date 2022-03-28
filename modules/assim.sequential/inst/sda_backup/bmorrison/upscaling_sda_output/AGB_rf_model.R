library(raster)
library(shapefiles)
library(rgdal)
library(PEcAn.data.remote)
library(PEcAn.all)
library(PEcAn.SIPNET)
library(PEcAn.LINKAGES)
library(PEcAn.visualization)
library(PEcAn.assim.sequential)
library(nimble)
library(lubridate)
library(PEcAn.visualization)
#PEcAn.assim.sequential::
library(rgdal) # need to put in assim.sequential
library(ncdf4) # need to put in assim.sequential
library(purrr)
library(listviewer)
library(dplyr)
library(furrr)
library(tictoc)

analysis = readRDS('/data2/bmorrison/sda/500_site_run/output_folder/ANALYSIS.RDS')
forecast = readRDS('/data2/bmorrison/sda/500_site_run/output_folder/FORECAST.RDS')

dates = names(analysis)
sites = unique(attributes(analysis[[names(analysis)[1]]])$Site)
observations = sites


obs = observations
working = print(paste("working on: ", i))
sites = print(obs)
PEcAn.logger::logger.info("**** Extracting LandTrendr AGB data for model sites ****")
bety <- list(user='bety', password='bety', host='localhost',
             dbname='bety', driver='PostgreSQL',write=TRUE)
con <- PEcAn.DB::db.open(bety)
bety$con <- con
site_ID <- obs
suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
                                              ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
                                            ids = site_ID, .con = con))

suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
site_info <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat,
                  lon=qry_results$lon, time_zone=qry_results$time_zone)


# this is a super crude way to make the dataframe with median values + .5 and .95 quantiles for analysis and forecast.
analysis_data = data.frame()
for (i in 1:length(dates))
{
  data = analysis[[dates[i]]]
  agb_data = data[,which(colnames(data) == "AbvGrndWood")]
  agb_a = as.vector(apply(agb_data, 2, FUN = median, na.rm = T))
  
  lai_data = data[,which(colnames(data) == "LAI")]
  lai_a = as.vector(apply(lai_data, 2, FUN = median))
  
  soil_data = data[,which(colnames(data) == "TotSoilCarb")]
  soil_a = as.vector(apply(soil_data, 2, FUN = median))
  
  data = forecast[[dates[i]]]
  agb_data =  data[,which(colnames(data) == "AbvGrndWood")]
  agb_f = as.vector(apply(agb_data, 2, FUN = median, na.rm = T))
  
  lai_data = data[,which(colnames(data) == "LAI")]
  lai_f = as.vector(apply(lai_data, 2, FUN = median))
  
  soil_data = data[,which(colnames(data) == "TotSoilCarb")]
  soil_f = as.vector(apply(soil_data, 2, FUN = median))
  
  info = as.data.frame(cbind(site_info$site_id, dates[i], site_info$lon, site_info$lat, agb_a,  lai_a,  soil_a, 
                             agb_f,  lai_f, soil_f), stringsAsFactors = F)
  names(info) = c("site_id", "date", "lon", "lat", "agb_a",  "lai_a", "soil_a",  
                  "agb_f",  "lai_f",  "soil_f")
  analysis_data = rbind(analysis_data, info, stringsAsFactors = F)
}
analysis_data$date = as.numeric(substr(analysis_data$date, 1, 4))
analysis_data$lon = as.numeric(analysis_data$lon)
analysis_data$lat = as.numeric(analysis_data$lat)
analysis_data$agb_a = as.numeric(analysis_data$agb_a)

analysis_data$lai_a = as.numeric(analysis_data$lai_a)

analysis_data$soil_a = as.numeric(analysis_data$soil_a)

analysis_data$agb_f = as.numeric(analysis_data$agb_f)

analysis_data$lai_f = as.numeric(analysis_data$lai_f)

analysis_data$soil_f = as.numeric(analysis_data$soil_f)

# lt = shapefile('/data/bmorrison/sda/500_site_run/Landtrendr_AGB_493_sites/lt_agb.shp')
# lt = as.data.frame(lt)
# lt$Date = as.numeric(substr(lt$Date, 6, 10))
# analysis_data$lt = NA
# for (i in 1:nrow(analysis_data))
# {
#   index = which(lt$coords.x1 == analysis_data$lon[i] & lt$coords.x2 == analysis_data$lat[i] & lt$Date == analysis_data$date[i])
#   if (length(index) > 0)
#   {
#     analysis_data$lt[i] = lt$Median[index]
#   }
# }

pft = shapefile('/data/bmorrison/sda/500_site_run/SDA_500_Soil.shp')
pft = as.data.frame(pft)
analysis_data$pft = NA
sites = unique(analysis_data$site_id)
for (i in 1:length(sites))
{
  index_sites = which(analysis_data$site_id == sites[i])
  index_data = which(analysis_data$lon[index_sites][1] == pft$coords.x1 | analysis_data$lat[index_sites][1] == pft$coords.x2)
  analysis_data$pft[index_sites] = pft$pft[index_data][1]
}


eco = shapefile('/data2/bmorrison/sda/bailey_paper/data/ecoregions_shapefile/eco_aea_l1.shp')
test = extract(eco, cbind(analysis_data$lon, analysis_data$lat))

analysis_data$pft = as.factor(analysis_data$pft)
analysis_data$eco = as.factor(test$NA_L1CODE)


# add in climate data
# files = list.files(path = '/data2/bmorrison/sda/bailey_paper/data/climate/', pattern = '.tif', include.dirs = T, full.names = T)
# files = files[c(2,7,18,23,27, 32)]
clim = stack(c('/data2/bmorrison/prism_climate/annuals/aet_annual.tif', '/data2/bmorrison/prism_climate/annuals/def_annual.tif'))
names(clim) = c("aet", "def")

t = as.data.frame(extract(clim, cbind(analysis_data$lon, analysis_data$lat)))

analysis_data$aet = t$aet
analysis_data$def = t$def
# analysis_data$rain = t$rain
# analysis_data$snow = t$snow
# analysis_data$tmax = t$tmax
# analysis_data$tmin = t$tmin

elev = raster('/data2/bmorrison/prism_climate/normals/elevation/PRISM_us_dem_4km_bil.bil')
analysis_data$elev = as.numeric(extract(elev, cbind(analysis_data$lon, analysis_data$lat)))
#outlier analysis
sd = sd(analysis_data$agb_a, na.rm = T)
z = abs(analysis_data$agb_a - mean(analysis_data$agb_a, na.rm = T))/sd
bad = which(z >= 3)

data = analysis_data[-bad,]
# data = analysis_data
# year = 2012
# index = which(data$date == year)
# data = data[index,]

s = sample(1:nrow(data), round(.8*nrow(data)))
training = data[s,]
testing = data[-s,]


# stratified sampling for the different agb values)
min = min(training$agb_a, na.rm = T)
max = max(training$agb_a, na.rm = T)
bin_width = (max-min)/20
bins = seq(from = min, to = max, by = bin_width)
training_bins = cut(training$agb_a, breaks = bins, include.lowest = T)
training$bin = training_bins


bins = sort(unique(training$bin))
nrows = 20000
nsamples = round(nrows/length(unique(training$bin)), digits = 0)

training_dataset = data.frame()
for (i in 1:length(bins))
{
  index = which(training$bin == bins[i])
  s = sample(1:length(index), nsamples, replace = T)
  d = training[index[s],]
  training_dataset = rbind(training_dataset, d)
}

library(mgcv)
# test = gam(agb_a ~ s(lon, lat) + s(elev) + s(aet) + s(def) , data = training_dataset, method = "REML")
# 
# pr = as.numeric(predict(test, newdata = testing, na.action = na.pass))
# cor(testing$agb_a, pr, use = "complete.obs")

library(randomForest)
rf1 = randomForest(agb_a ~ aet+def+elev, data = training_dataset, na.action = na.omit)

pr = as.numeric(predict(rf1, newdata = testing, na.action = na.pass))

cor(testing$agb_a, pr, use = "complete.obs")

max = max(pr, testing$agb_a, na.rm = T)
par(mfrow = c(1,2))
varImpPlot(rf1, main = as.character(rf1$call)[2])
plot(testing$agb_a, pr, xlim = c(0,max), ylim = c(0,max), xlab = "SDA AGB", ylab = "Predicted AGB", main = "", col = 'black')
abline(0,1, col = 'red')

save(rf1, file = '/data2/bmorrison/sda/500_site_run/output_folder/agb_all_dates_rf_model.RData')
test = analysis_data[analysis_data$date == 2012,]

pr2 = pr = as.numeric(predict(rf1, newdata = test, na.action = na.pass))
cor(test$agb_a, pr2, use = "complete.obs")

max = max(pr2, test$agb_a, na.rm = T)
par(mfrow = c(1,2))
varImpPlot(rf1, main = as.character(rf1$call)[2])
plot(test$agb_a, pr2, xlim = c(0,max), ylim = c(0,max), xlab = "SDA AGB", ylab = "Predicted AGB", main = "2012", col = 'black')
abline(0,1, col = 'red')


library(Metrics)
rmse(testing$agb_a, pr)

# percent bias
sum(testing_agb_a-pr, na.rm = T)/length(pr)


