library(raster)
library(shapefiles)
library(rgeos)
library(rgeos)
library(sp)

analysis = readRDS('/data2/bmorrison/sda/500_site_run/output_folder/ANALYSIS.RDS')
sites = unique(attributes(analysis[[names(analysis)[1]]])$Site)
obs = sites

load('/data2/bmorrison/sda/500_site_run/output_folder/500_site_run_SITE_INFO.Rdata')
sda_500 = site_info
#data
load('/data2/bmorrison/sda/500_site_run/output_folder/sites_with_lai_no_duplicates_SITE_INFO.Rdata')
sda_lai = data


#site_info
#load(file = '/data2/bmorrison/sda/500_site_run/output_folder/PFT_site_data.Rdata')
load('/data/bmorrison/sda/500_site_run/pft_site_types.RData')
pft_types = site_data

index = which(sda_500$site_id %in% pft_types$ID)

sda_500 = sda_500[index,]
sda_500$pft = NA
for (i in 1:nrow(sda_500))
{
  index = which(pft_types$ID == sda_500$site_id[i])
  sda_500$pft[i] = pft_types$pft[index]
}

bad = c(which(is.na(sda_500$pft)), which(sda_500$pft == "NA"))
sda_500 = sda_500[-bad,]

sda_500$class = sda_500$pft
sda_500$class[sda_500$pft == "mesic grassland"] = "blue"
sda_500$class[sda_500$pft == "arid grassland"] = "goldenrod2"
sda_500$class[sda_500$pft == "conifer"] = "chartreuse4"
sda_500$class[sda_500$pft == "deciduous"] = "green"
sda_500$class[sda_500$pft == "developed"] = "red"

coordinates(sda_500) = ~lon+lat
projection(sda_500) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "


us = shapefile('/data2/bmorrison/sda/bailey_paper/data/cb_2018_us_nation_20m/us_boarder_reproject.shp')
us = spTransform(us, CRS = crs(sda_500))

pfts = unique(sda_500$pft)
names = toupper(pfts)
jpeg('/data2/bmorrison/sda/500_site_run/manuscript/figures/pft_site_locations.jpeg', width = 8.5, height = 4, res = 300, units ="in")
par(mfrow = c(2,3), mai = c(0.1, 0.1, .5, 0.1))
for (i in 1:length(pfts))
{
  index = which(sda_500$pft == pfts[i])
  d = sda_500[index,]
  plot(us, main = names[i])
  plot(d, add = T, pch = 20, col = d$class)
}
dev.off()

jpeg('/data2/bmorrison/sda/500_site_run/manuscript/figures/All_pft_site_locations_1_plot.jpeg', width = 8.5, height = 6, res = 300, units ="in")
plot(us)
plot(sda_500, col = sda_500$class, pch = 20, add = T)
legend("bottomleft", c("Mesic Grassland", "Arid Grassland", "Conifer", "Deciduous", "Developed"), col = c("blue", "goldenrod2", "chartreuse4", "green", "red"), 
       pch = 20,  bty = 'n', cex = 1.1)
dev.off()

save(sda_500, file = '/data2/bmorrison/sda/500_site_run/manuscript/sda_500_pft_information.Rdata')







site_info = site_info[complete.cases(site_info),]
bad = which(site_info$pft == "NA")
site_info = site_info[-bad,]
index = which(sda_500$site_id %in% site_info$site_id)
sda_500 = sda_500[index,]
sda_500$pft = NA
for (i in 1:nrow(sda_500))
{
  index = which(site_info$site_id == sda_500$site_id[i])
  sda_500$pft[i] = site_info$pft[index]
}

sda_500$class = sda_500$pft
sda_500$class[sda_500$pft == "mesic grassland"] = "blue"
sda_500$class[sda_500$pft == "arid grassland"] = "goldenrod2"
sda_500$class[sda_500$pft == "conifer"] = "chartreuse4"
sda_500$class[sda_500$pft == "deciduous"] = "green"
sda_500$class[sda_500$pft == "developed"] = "red"



coordinates(sda_500) = ~lon+lat
coordinates(sda_lai) = ~lon+lat
projection(sda_500) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
projection(sda_lai) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "

us = shapefile('/data2/bmorrison/sda/bailey_paper/data/cb_2018_us_nation_20m/us_boarder_reproject.shp')
us = spTransform(us, CRS = crs(sda_500))

pfts = toupper(unique(sda_500$pft))
par(mfrow = c(2,3))
for (i in 1:length(pfts))
{
  index = which(sda_500$pft == tolower(pfts[i]))
  d = sda_500[index,]
  plot(us)
  plot(d, add = T, pch = 20, col = d$class, main = pfts[i])
}
jpeg('/data2/bmorrison/sda/500_site_run/manuscript/figures/pft_site_locations.jpeg', width = 8.5, height = 7, res = 300, units ="in")
plot(us)
plot(sda_500, col = sda_500$class, pch = 20, add = T)
legend("bottomleft", c("Mesic Grassland", "Arid Grassland", "Conifer", "Deciduous", "Developed"), col = c("blue", "goldenrod2", "chartreuse4", "green", "red"), 
       pch = 20,  bty = 'n', cex = 1.5)
dev.off()



