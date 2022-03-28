library(raster)
r = raster('/data/bmorrison/sda/bailey_paper/data/landcover/NLCD_2016_Land_Cover_L48_20190424.img')

###################################################################################################
#################### TILE AND REMOVE NON-NATURAL LOCATIONS FROM RASTER ############################

ex = extent(r)
ncol = ncol(r)
nrow = nrow(r)
row_pix = 10000
col_pix = 10000
nc = floor(ncol/col_pix)
nr = floor(nrow/row_pix)
# plot(r)
for (i in 1:(nc+1))
{
  if (i == 1)
  {
    start_col = 1
    end_col = i * col_pix
  }
  if (i == nc+1)
  {
    end_col = ncol
    start_col = i * col_pix
  }
  
  if (i > 1 & i < nc+1)
  {
    end_col = i*col_pix
    start_col = end_col - col_pix
  }
  #print(paste(start_col, end_col))

  for (j in 1:(nr+1))
  {
    if (j == 1)
    {
      start_row = 1
      end_row = start_row + row_pix - 1
    }
    if (j == nr+1)
    {
      end_row = nrow
      start_row = j * row_pix
    }
    
    if (j > 1 & j < nr+1)
    {
      end_row = j*row_pix
      start_row = end_row - row_pix
    }
    
    #print(paste( start_row, end_row))
    
    ext = extent(xFromCol(r, start_col), xFromCol(r, end_col), yFromRow(r, end_row), yFromRow(r, start_row))
    # points = rbind(c(ext[1], ext[3]), c(ext[1], ext[4]), c(ext[2], ext[3]), c(ext[2], ext[4]))
    # points(points, add = T, pch = 19, col = 'red')
    # text(x = ext[1], y = ext[3]+10000, paste(i, j, sep = "_"), cex = 1.5, col = 'red', adj = 0.5, pos = 4)

    lib1 = print("library(raster)")
    rast = print("r = raster('/data/bmorrison/sda/bailey_paper/data/landcover/NLCD_2016_Land_Cover_L48_20190424.img')")
    crop = print(paste("c = crop(r, extent(",ext[1], ",", ext[2], ",", ext[3], ",", ext[4], "))", sep = ""))

    crop2 = print("c[c[] == 0] = NA")
    crop3 = print("c[c[] <= 31 | c[] > 95 ] = NA")
    write_raster = print(paste("writeRaster(c, file = '/data/bmorrison/sda/bailey_paper/data/landcover/tiles/NLCD_corrected_2016_", i, "_", j, ".tif', overwrite = T)", sep = ""))

    out = c(lib1, rast, crop, crop2, crop3,write_raster)
    write(out, file = paste("/data/bmorrison/sda/bailey_paper/data/landcover/tiles/qsub_files/r_", i, "_", j, ".R", sep = ""))

    
  }
}

# ###################################################################################################
# #################### REVERSE MASK FOR NON_NATURAL LOCATIONS #######################################

library(raster)
r = raster("/data/bmorrison/sda/bailey_paper/data/landcover/NLCD_2016_Land_Cover_L48_20190424.img")

ex = extent(r)
ncol = ncol(r)
nrow = nrow(r)
row_pix = 10000
col_pix = 10000
nc = floor(ncol/col_pix)
nr = floor(nrow/row_pix)


# plot(r)
for (i in 1:(nc+1))
{
  if (i == 1)
  {
    start_col = 1
    end_col = i * col_pix
  }
  if (i == nc+1)
  {
    end_col = ncol
    start_col = i * col_pix
  }
  
  if (i > 1 & i < nc+1)
  {
    end_col = i*col_pix
    start_col = end_col - col_pix
  }
  #print(paste(start_col, end_col))
  
  for (j in 1:(nr+1))
  {
    if (j == 1)
    {
      start_row = 1
      end_row = start_row + row_pix - 1
    }
    if (j == nr+1)
    {
      end_row = nrow
      start_row = j * row_pix
    }
    
    if (j > 1 & j < nr+1)
    {
      end_row = j*row_pix
      start_row = end_row - row_pix
    }
    
    #print(paste( start_row, end_row))
    
    ext = extent(xFromCol(r, start_col), xFromCol(r, end_col), yFromRow(r, end_row), yFromRow(r, start_row))
    # points = rbind(c(ext[1], ext[3]), c(ext[1], ext[4]), c(ext[2], ext[3]), c(ext[2], ext[4]))
    # points(points, add = T, pch = 19, col = 'red')
    # text(x = ext[1], y = ext[3]+10000, paste(i, j, sep = "_"), cex = 1.5, col = 'red', adj = 0.5, pos = 4)
    
    lib1 = print("library(raster)")
    rast = print("r = raster('/data/bmorrison/sda/bailey_paper/data/landcover/NLCD_2016_Land_Cover_L48_20190424.img')")
    crop = print(paste("c = crop(r, extent(",ext[1], ",", ext[2], ",", ext[3], ",", ext[4], "))", sep = ""))

    crop2 = print("c[c[] == 0] = NA")
    crop3 = print("c[c[] == 11 | c[] == 12 | c[] == 21 | c[] == 22 | c[] == 23 | c[] == 24 | c[] == 31] = 0")
    crop4 = print("c[c[] == 41 | c[] == 42| c[] == 43 | c[] == 52 | c[] == 71 | c[] ==81 | c[] == 82 | c[] == 90 | c[] == 95] = 1")
    write_raster = print(paste("writeRaster(c, file = '/data/bmorrison/sda/bailey_paper/data/landcover/tiles/NLCD_corrected_2016_mask_", i, "_", j, ".tif', overwrite = T)", sep = ""))

    out = c(lib1, rast, crop, crop2, crop3,crop4, write_raster)
    write(out, file = paste("/data/bmorrison/sda/bailey_paper/data/landcover/tiles/qsub_files/r_", i, "_", j, ".R", sep = ""))
    
  }
}


##### Bring in climate files to look at unique groups
library(gdalUtils)
library(raster)
library(factoextra)
library(NbClust)
library(tidyverse)
library(gridExtra)
library(scatterplot3d)
library(rgl)
setwd('/data/bmorrison/sda/bailey_paper/data/climate/')
files1 = list.files(path = '/data/bmorrison/sda/bailey_paper/data/climate/', pattern = 'annual')[-4]
files2 = c("tmax_max.tif", "tmin_min.tif")
files = c(files1, files2)
# summer = stack(list.files(path = '/data/bmorrison/sda/bailey_paper/data/climate/', pattern = 'summer', include.dirs = T, full.names = T))
#winter = stack(list.files(path = '/data/bmorrison/sda/bailey_paper/data/climate/', pattern = 'winter', include.dirs = T, full.names = T))
landcover = raster('/data/bmorrison/sda/bailey_paper/data/landcover/landcover_2016_aligned.tif')
mask = landcover
mask[mask[]>0] = 1
mask[mask[] != 1] = NA

#mask = raster('/data/bmorrison/sda/bailey_paper/data/landcover/landcover_mask_reverse_aligned.tif')
#mask[mask[] == 0] = 1

#climate = stack(summer, winter, landcover)
climate = stack(files, landcover)
climate = climate*mask
names(climate) = c("aet", "def", "rad", "rain", "snow", "tmax", "tmin", "landcover")
#names(climate) = c("aet_s", "def_s", "rad_s", "rain_s", "snow_s", "tmax_s", "tmin_s", "aet_w", "def_w", "rad_w", "rain_w", "snow_w", "tmax_w", "tmin_w", "landcover")

#climate = subset(climate, c(1,2,4,5,6, 7, 8, 9, 11, 12, 13, 14, 15))
ecoregions = raster('/data/bmorrison/sda/bailey_paper/data/ecoregion_l3.tif')
# ecoregions = shapefile('/data/bmorrison/sda/bailey_paper/data/ecoregions_shapefile/eco_aea.shp')
# ecoregions = spTransform(ecoregions, CRS = crs(climate))
us = shapefile('/data/bmorrison/sda/bailey_paper/data/cb_2018_us_nation_20m/cb_2018_us_nation_20m.shp')
us = crop(spTransform(us, CRS = crs(climate)), extent(climate))
us = rasterize(us, ecoregions, field = 1)
eco = ecoregions * us

clusters = eco * NA

regions_index = unique(ecoregion$CODE)
#regions_index = regions[-1]
regions = unique(ecoregion$NA_L3CODE)
regions_code = unique(ecoregion$NA_L3CODE)
regions = grep('^[1][0].[1]', regions)
regions = regions_index[regions]
for (i in 1:length(regions))
{
  index = which(eco[] == regions[i])
  if (length(index)>0)
  {
    xy = xyFromCell(eco, cell = index)
    data = as.data.frame(raster::extract(climate, index))
    data$index = index
    #data = cbind(data, xy)
    nas = which(is.na(data[,1]))
    data = as.data.frame(data[complete.cases(data),])
    
    data.scale = as.data.frame(scale(data[1:8]))
    
    data.scale = data.scale[complete.cases(data.scale),]
    dim(data)
    dim(data.scale)
    print(regions_code[regions[i]])
  }
  print(dim(data.scale))
  
  k2 = hkmeans(data.scale, 2, hc.metric = "euclidean", iter.max = 10)
  k3 = hkmeans(data.scale, 3, hc.metric = "euclidean", iter.max = 50)
  k4 = hkmeans(data.scale, 4, hc.metric = "euclidean", iter.max = 50)
  k5 = hkmeans(data.scale, 5, hc.metric = "euclidean", iter.max = 50)
  k6 = hkmeans(data.scale, 6, hc.metric = "euclidean", iter.max = 50)
  # k7 = hkmeans(data.scale, 7, hc.metric = "euclidean", iter.max = 50)
  # k8 = hkmeans(data.scale, 8, hc.metric = "euclidean", iter.max = 50)
  # k9 = hkmeans(data.scale, 9, hc.metric = "euclidean", iter.max = 50)
  # k10 = hkmeans(data.scale, 10, hc.metric = "euclidean", iter.max = 50)
  
  
  p1 = fviz_cluster(k2, data = data.scale, labelsize = 0)
  p2 = fviz_cluster(k3, data = data.scale, labelsize = 0)
  p3 = fviz_cluster(k4, data = data.scale, labelsize = 0)
  p4 = fviz_cluster(k5, data = data.scale, labelsize = 0)
  p5 = fviz_cluster(k6, data = data.scale, labelsize = 0)
  # p6 = fviz_cluster(k7, data = data.scale, labelsize = 0)
  # p7 = fviz_cluster(k8, data = data.scale, labelsize = 0)
  # p8 = fviz_cluster(k9, data = data.scale, labelsize = 0)
  # p9 = fviz_cluster(k10, data = data.scale, labelsize = 0)
 
  
  #grid.arrange( p9,p10, p11,p12,p13,nrow = 2) # )
  
  grid.arrange(p1, p2, p3,p4, p5,  nrow = 2) # )
  
  pdf(file = '/data/bmorrison/sda/bailey_paper/data/clusters/snake_river_plain_1018_cluster_plots.pdf', width = 16, height = 8)
  grid.arrange(p1, p2, p3,p4, p5,nrow = 2) #p7, p8, p9, nrow = 3)
  dev.off()
  
  clusters[data$index] = k4$cluster+186
  
  # plot3d(x = data.scale$tmax_s, y = data.scale$aet_s, z = data.scale$landcover, col = k2$cluster)
  # scatterplot3d(x = data.scale$tmax_s, y = data.scale$aet_s, z = data.scale$landcover, color = k2$cluster,  pch = 19)
  
 
}

# library(ppcor)
# cors = pcor(data)
# estimates = cors$estimate
# diag(estimates) = NA
# pvals = cors$p.value
# diag(pvals) = NA
# colnames(pvals) = c(names(climate), "x", "y")
# rownames(pvals) = c(names(climate), "x", "y")
# 
# bad = which(abs(pvals) > 0.05)

# pairs(data)


# s = sample(1:nrow(data.scale), 5000)
# d = data[s,]


# k1 <- kmeans(d, centers = 2, nstart = 25)
# k2 <- kmeans(d, centers = 3, nstart = 25)
# k2 <- kmeans(d, centers = 4, nstart = 25)

# p1 <- d %>%
#   as_tibble() %>%
#   mutate(cluster = k1$cluster,
#          state = row.names(d)) %>%
#   ggplot(aes(x, y, color = factor(cluster), label = state)) +
#   geom_text()
# 
# # p2 <- d %>%
# #   as_tibble() %>%
# #   mutate(cluster = k1$cluster,
# #          state = row.names(d)) %>%
# #   ggplot(aes(aet_s, tmin_w, color = factor(cluster), label = state)) +
# #   geom_text()
# # 
# # p3 <- d %>%
# #   as_tibble() %>%
# #   mutate(cluster = k1$cluster,
# #          state = row.names(d)) %>%
# #   ggplot(aes(def_s, rad_s, color = factor(cluster), label = state)) +
# #   geom_text()
# p2 <- d %>%
#   as_tibble() %>%
#   mutate(cluster = k2$cluster,
#          state = row.names(d)) %>%
#   ggplot(aes(x, y, color = factor(cluster), label = state)) +
#   geom_text()
# p3 <- d %>%
#   as_tibble() %>%
#   mutate(cluster = k2$cluster,
#          state = row.names(d)) %>%
#   ggplot(aes(x, y, color = factor(cluster), label = state)) +
#   geom_text()
# 
# library(gridExtra)
# grid.arrange(p1, p2, p3,nrow = 1)
# 
# 
# 

# Elbow method
# fviz_nbclust(d, kmeans, k.max = 20, method = "wss") +
#   labs(subtitle = "Elbow method")
# 
# # Silhouette method
# fviz_nbclust(d, kmeans, k.max = 20, method = "silhouette")+
#   labs(subtitle = "Silhouette method")
# 
# 
# set.seed(123)
# fviz_nbclust(d, kmeans, nstart = 25,  k.max = 100, method = "gap_stat", nboot = 50)+
#   labs(subtitle = "Gap statistic method")
