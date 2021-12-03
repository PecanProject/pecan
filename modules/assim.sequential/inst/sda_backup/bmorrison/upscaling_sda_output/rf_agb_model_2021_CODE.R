library(raster)
library(randomForest)
library(shapefiles)

# get input data setup
ndvi = raster('/data2/bmorrison/sda/500_site_run/data/MODIS_Veg/NDVI_2012_aligned.tif')
evi = raster('/data2/bmorrison/sda/500_site_run/data/MODIS_Veg/EVI_2012_aligned.tif')

clim_files = list.files(path = '/data2/bmorrison/sda/bailey_paper/data/climate/', pattern = "annual", include.dirs = T, full.names = T)
clim_files = clim_files[c(2,4,5, 9, 11)]
clim_files = c(clim_files, "/data2/bmorrison/sda/bailey_paper/data/climate/tmax_max.tif", "/data2/bmorrison/sda/bailey_paper/data/climate/tmin_min.tif", "/data2/bmorrison/prism_climate/annuals/elevation.tif")

s = stack(c(ndvi, evi, clim_files))
mask = raster("/data2/bmorrison/sda/500_site_run/data/MODIS_Veg/mask.tif")
s = s*mask
names(s) = c("ndvi", "evi", "aet", "def", "rad", "rain", "snow", "tmax", "tmin", "elevation")
projection(s) = crs(ndvi)


# pull in the SDA run AGB data
analysis = readRDS('/Volumes/data2/bmorrison/sda/500_site_run/output_folder/ANALYSIS.RDS')
sites = unique(attributes(analysis[[names(analysis)[1]]])$Site)
obs = sites
bety <- list(user='bety', password='bety', host='modex.bnl.gov',
             dbname='betydb', driver='PostgreSQL',write=TRUE)
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

# site_info  = as.data.frame(cbind(site_info$site_id, site_info$lon, site_info$lat))
# names(site_info) = c("site_id", "lon", "lat")
# save(site_info, file = '/Volumes/data2/bmorrison/sda/500_site_run/output_folder/500_site_run_SITE_INFO.Rdata')


load('/data2/bmorrison/sda/500_site_run/output_folder/500_site_run_SITE_INFO.Rdata')

date = 2012
index = which(names(analysis) == "2012/07/15")
data = analysis[[index]]
data= data[,which(names(data) == "AbvGrndWood")]

biomass = apply(data, 2, FUN = median, na.rm = T)
biomass = as.data.frame(cbind(sites, biomass))
row.names(biomass) = NULL
names(biomass) = c("site_id", "biomass")
biomass_data = biomass
biomass_data$lon = NA
biomass_data$lat = NA
for (i in 1:nrow(biomass_data))
{
  id = biomass_data$site_id[i]
  index = which(site_info$site_id == id)
  if (length(index) > 0)
  {
    biomass_data$lon[i] = site_info$lon[index]
    biomass_data$lat[i] = site_info$lat[index]
  }
}


coordinates(biomass_data) = ~lon+lat
projection(biomass_data) = crs(s)

par(mfrow = c(1,1))
plot(ndvi, col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL))
plot(biomass_data, pch = 20, col = terrain.colors(biomass_data$biomass), add = T)

# prepare training and testing datasets
input_data = as.data.frame(extract(s, biomass_data))
input_data = cbind(input_data, biomass_data$biomass)
names(input_data) = c("ndvi", "evi", "aet", "def", "rad", "rain", "snow", "tmax", "tmin", "elevation", "biomass")
input_data$biomass = as.numeric(input_data$biomass)
#### outlier analysis ####
# sd = sd(input_data$biomass)
# z = abs(input_data$biomass-mean(input_data$biomass))/sd
# index = which(z > 2.5)
# input_data = input_data[-index,]

samples = sample(1:nrow(input_data), .8*nrow(input_data))
training = input_data[samples,]
testing = input_data[-samples,]

# make the model!!!
rf = randomForest(biomass~ndvi+evi+aet+def+rain+snow+tmax+elevation, 
                  data = training, ntree = 1000, na.action = na.omit, keep.forest = T)

pr = predict(rf, newdata = testing, na.action = na.pass)
max = max(pr, testing$biomass)
jpeg('/data2/bmorrison/sda/500_site_run/rf_model_diagnostics_2021.jpeg', height = 8, width = 8, units = "in", res = 300)
par(mfrow = c(2,2))
plot(rf, main = "Tree Error")
varImpPlot(rf, main = "Variable Importance")
plot(testing$biomass, pr, xlim = c(0, max), ylim = c(0, max), xlab = "SDA AGB", ylab = "Predicted", main = "Obs vs. Predicted")
abline(0, 1, col = 'red')

c = round(cor(pr, testing$biomass, use ="complete.obs"), digits = 1)
rmse = function(obs, pred)
{
  error = sqrt(sum((pred-obs)^2)/length(pred))
  return(error)
}
error = round(rmse(testing$biomass, pr), digits = 1)
library(hydroGOF)
pb = pbias(testing$biomass, pr, na.rm = T)
text(x = 0, y = 200, labels = paste0("Cor=",c), pos = 4)
text(x = 0, y = 190, labels = paste0("RMSE=", error), pos = 4)
text(x = 0, y = 180, labels = paste0("%Bias=",pb), pos = 4)

test = predict(object = s, model = rf, na.rm = T)
plot(test, main = "CONUS AGB Estimate")
dev.off()

##### Make STDEV estimate from model
na_index = which(is.na(s[]))
d = as.data.frame(s)
d$cell = 1:nrow(d)
d = d[-na_index,]
test2 = predict(rf, newdata = d, na.action = na.omit, predict.all = T)
test2_data = test2$individual
test2_sd = apply(test2_data, 1, FUN = sd, na.rm = T)

test2 = mask*NA
test2[d$cell] = test2_sd

lt_agb = raster("/data2/bmorrison/sda/500_site_run/data/landtrendr_agb_2012_800m.tif")
lt_agb_se = raster("/data2/bmorrison/sda/500_site_run/data/landtrendr_agb_stdev_2012_800m.tif")
agb_diff = abs(lt_agb-test)
se_diff = abs(lt_agb_se-test2)
# range_lt = range(r[], na.rm = T)
# breaks_lt = seq(range_lt[1], range_lt[2], by = 31)
breaks_agb = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600)
breaks_stdev = c(0, 25, 50, 75, 100, 125, 150)
breaks_diff = c()
jpeg('/data2/bmorrison/sda/500_site_run/rf_model_comparison_2021.jpeg', height = 10, width = 14, units = "in", res = 300)
par(mfrow = c(2,3))
plot(lt_agb, col = rev(terrain.colors(length(breaks_agb)-1)), breaks = breaks_agb, main = "LandTrendr AGB 800m")
plot(lt_agb_se, col = rev(terrain.colors(length(breaks_stdev)-1)), breaks = breaks_stdev, main = "LandTrendr STDEV 800m")
plot(agb_diff,  main = "Difference LT vs. RF AGB Estimates")
plot(test, col = rev(terrain.colors(length(breaks_agb)-1)), breaks = breaks_agb, main = "SDA RF AGB")
plot(test2, col = rev(terrain.colors(length(breaks_stdev)-1)), breaks = breaks_stdev, main = "SDA RF STDEV")
plot(se_diff,  main = "Difference LT vs. RF STDEV Estimates")
dev.off()
