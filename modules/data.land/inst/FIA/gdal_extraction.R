# setwd("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/soil_nc/")
# igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
# sg_url="/vsicurl/https://files.isric.org/soilgrids/latest/data/ocd/ocd_"
# depths <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
# data_tag <- c("_mean.vrt", "_Q0.05.vrt", "_Q0.5.vrt", "_Q0.95.vrt")
# name_tag <- expand.grid(depths, data_tag, stringsAsFactors = F)#find the combinations between data and depth tags.
# L <- split(as.data.frame(name_tag), seq(nrow(as.data.frame(name_tag))))
# 
# for (i in seq_along(L)) {
#   ocd_url <- paste0(sg_url, L[[i]][[1]], L[[i]][[2]])
#   gdalUtilities::gdal_translate(ocd_url, file.path("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/soil_nc/soilgrids_250m", gsub(".vrt", ".tif", basename(ocd_url)))
#                                 # tr=c(250,250),
#                                 # projwin=ext,
#                                 # projwin_srs =igh
#                                 )
# }
# `gdalwarp -t_srs EPSG:4326 --config GDAL_CACHEMAX 5000 -multi -wm 90% -wo NUM_THREADS=25 -co BIGTIFF=YES -co COMPRESS=DEFLATE -co TILED=TRUE "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/ocd/ocd_0-5cm_mean.vrt" "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/soil_nc/soilgrids_250m/soc_0-5cm_mean.tif"`
# 
# "wget https://files.isric.org/soilgrids/latest/data/ocd/ocd_0-5cm_mean/ -r -np -nH --cut-dirs=3 -nc"
# "wget https://files.isric.org/soilgrids/latest/data/ocd/ocd_0-5cm_mean.vrt -O /projectnb/dietzelab/dongchen/anchorSites/NA_runs/soil_nc/soilgrids_250m/ocd/ocd_0-5cm_mean.vrt"
# 
library(purrr)
setwd("/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/")
U="https://files.isric.org/soilgrids/latest/data"
depths <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
data_tag <- c("_mean.vrt", "_Q0.05.vrt", "_Q0.5.vrt", "_Q0.95.vrt")
name_tag <- expand.grid(depths, data_tag, stringsAsFactors = F)#find the combinations between data and depth tags.
L <- split(as.data.frame(name_tag), seq(nrow(as.data.frame(name_tag))))
var.tags <- c("ocd", "clay", "sand", "silt", "soc", "bdod")
base.dir <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soilgrids_250m/"
if (!file.exists(base.dir)) {
  dir.create(base.dir)
}
for (v in var.tags) {
  dir <- file.path(base.dir, v)
  if (!file.exists(dir)) {
    print(v)
    dir.create(dir)
  } else {
    print("Folder exists, try downloading missing layers!")
    # next
  }
  sg_url <- paste0(file.path(U, v, v), "_")
  for (i in seq_along(L)) {
    url <- gsub(".vrt", "/", paste0(sg_url, L[[i]][[1]], L[[i]][[2]]))
    folder.path <- file.path(dir, basename(url))
    if (!file.exists(folder.path)) {
      dir.create(folder.path)
    } else {
      next
    }
    jobsh <- c("#!/bin/bash -l", 
               "cd @PATH@",
               "wget @URL@ -c -r -np -nH --cut-dirs=3 -nc",
               "wget @VRT_REMOTE@ -O @VRT@")
    jobsh <- gsub("@URL@", url, jobsh)
    jobsh <- gsub("@PATH@", folder.path, jobsh)
    jobsh <- gsub("@VRT_REMOTE@", paste0(sg_url, L[[i]][[1]], L[[i]][[2]]), jobsh)
    jobsh <- gsub("@VRT@", file.path(folder.path, v, paste0(basename(url), ".vrt")), jobsh)
    writeLines(jobsh, con = file.path(folder.path, "job.sh"))
    qsub <- "qsub -l h_rt=48:00:00 -l buyin -pe omp @CORES@ -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
    qsub <- gsub("@CORES@", 1, qsub)
    qsub <- gsub("@NAME@", paste0("Job-", i, "-", v), qsub)
    qsub <- gsub("@STDOUT@", file.path(folder.path, "stdout.log"), qsub)
    qsub <- gsub("@STDERR@", file.path(folder.path, "stderr.log"), qsub)
    qsub <- strsplit(qsub, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)
    cmd <- qsub[[1]]
    out <- system2(cmd, file.path(folder.path, "job.sh"), stdout = TRUE, stderr = TRUE)
  }
}

# gdalwarp
U="https://files.isric.org/soilgrids/latest/data"
depths <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
data_tag <- c("_mean.vrt", "_Q0.05.vrt", "_Q0.5.vrt", "_Q0.95.vrt")
name_tag <- expand.grid(depths, data_tag, stringsAsFactors = F)#find the combinations between data and depth tags.
L <- split(as.data.frame(name_tag), seq(nrow(as.data.frame(name_tag))))
var.tags <- c("ocd", "clay", "sand", "silt", "soc", "bdod")
base.dir <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soilgrids_250m/"
for (v in var.tags) {
  dir <- file.path(base.dir, v)
  sg_url <- paste0(file.path(U, v, v), "_")
  for (i in seq_along(L)) {
    url <- gsub(".vrt", "/", paste0(sg_url, L[[i]][[1]], L[[i]][[2]]))
    folder.path <- file.path(dir, basename(url), v)
    if (file.exists(file.path(folder.path, paste0(basename(url), ".tif")))) {
      print("GeoTIFF file exists, try generating next file!")
      next
    }
    if (!file.exists(file.path(folder.path, paste0(basename(url), ".vrt")))) {
      break
    }
    # create job file.
    jobsh <- c("#!/bin/bash -l", 
               "module load gdal", 
               "gdalwarp -t_srs EPSG:4326 --config GDAL_CACHEMAX 5000 -multi -wm 90% -wo NUM_THREADS=25 -co BIGTIFF=YES -co COMPRESS=DEFLATE -co TILED=TRUE @VRT@ @TIFF@")
    jobsh <- gsub("@VRT@", file.path(folder.path, paste0(basename(url), ".vrt")), jobsh)
    jobsh <- gsub("@TIFF@", file.path(folder.path, paste0(basename(url), ".tif")), jobsh)
    writeLines(jobsh, con = file.path(folder.path, "job.sh"))
    qsub <- "qsub -l h_rt=2:00:00 -l buyin -pe omp @CORES@ -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
    qsub <- gsub("@CORES@", 28, qsub)
    qsub <- gsub("@NAME@", paste0("Job-", i, "-", v), qsub)
    qsub <- gsub("@STDOUT@", file.path(folder.path, "stdout.log"), qsub)
    qsub <- gsub("@STDERR@", file.path(folder.path, "stderr.log"), qsub)
    qsub <- strsplit(qsub, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)
    cmd <- qsub[[1]]
    out <- system2(cmd, file.path(folder.path, "job.sh"), stdout = TRUE, stderr = TRUE)
  }
}

test <- readLines("/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soilgrids_250m/ocd/ocd_0-5cm_Q0.5/ocd/stderr.log")
length(test)

# checking error layers and missing tiles.
U="https://files.isric.org/soilgrids/latest/data"
base.dir <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soilgrids_250m/"
depths <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
data_tag <- c("_mean.vrt", "_Q0.05.vrt", "_Q0.5.vrt", "_Q0.95.vrt")
name_tag <- expand.grid(depths, data_tag, stringsAsFactors = F)#find the combinations between data and depth tags.
L <- split(as.data.frame(name_tag), seq(nrow(as.data.frame(name_tag))))
var.tags <- c("ocd", "clay", "sand", "silt", "soc", "bdod")
error.layers <- missing.files <- c()
for (v in var.tags) {
  dir <- file.path(base.dir, v)
  sg_url <- paste0(file.path(U, v, v), "_")
  for (i in seq_along(L)) {
    url <- gsub(".vrt", "/", paste0(sg_url, L[[i]][[1]], L[[i]][[2]]))
    folder.path <- file.path(dir, basename(url), v)
    if (!file.exists(file.path(folder.path, "stderr.log"))) {
      next
    }
    error <- readLines(file.path(folder.path, "stderr.log"))
    for (k in seq_along(error)) {
      missing.files <- c(missing.files, gsub(":", "", strsplit(error[k], " ", fixed = T)[[1]][3]))
    }
    if (length(error) > 0) {
      error.layers <- c(error.layers, file.path(dir, basename(url)))
    }
  }
}
# delete problematic folders.
unlink(error.layers, recursive = T)
# if (F) {
#   # download missing tiles.
#   for (tile in missing.files) {
#     splits <- strsplit(tile, "/./", fixed = T)[[1]]
#     folder.path <- splits[1]
#     tile.path <- splits[2]
#     # download.
#     v.flag <- var.tags %>% purrr::map(function(v){
#       grepl(v, tile.path)
#     }) %>% unlist %>% which
#     from <- file.path(U, var.tags[v.flag], tile.path)
#     to <- file.path(folder.path, tile.path)
#     if (!file.exists(to)) {
#       download.file(from, to)
#     }
#   }
#   # delete qsub and tif files.
#   for (file in error.layers) {
#     unlink(list.files(file, pattern = "*.tif|*.log", full.names = T))
#   }
# }