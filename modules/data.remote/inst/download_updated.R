#DAAC_Set_Credential(replace = TRUE)

# California bounding box is:
# up_lat <- 42.0095082699265845
# up_lon <- -124.4820168611238245
# low_lat <- 32.5288367369123748
# low_lon <- -114.1312224747231312

ul_lat <- 42.0095082699265845 # y = 4651894 in crs
ul_lon <- -124.4820168611238245 # x = 377279.7 in crs
lr_lat <- 32.5288367369123748 # y = 3633946 in crs
lr_lon <- -114.1312224747231312 # x = 1334269 in crs

from <- "2019-01-01"
to <- "2019-12-31"
doi <- "10.5067/HLS/HLSS30.002"
outdir <- "//projectnb/dietzelab/XinyuanJi/State_of_California_HLSS/2019_Fmask"
# SWIR - Landsat (B6&7), Sentinel (B11&12)
band <- "Fmask"
credential.folder <- "~/projectnb/XinyuanJi"
paths <- NASA_DAAC_download(ul_lat = ul_lat, 
                            ul_lon = ul_lon, 
                            lr_lat = lr_lat, 
                            lr_lon = lr_lon,
                            ncore = 16,
                            from = from, 
                            to = to,
                            outdir = outdir,
                            band = band,
                            credential.folder = credential.folder,
                            doi = doi,
                            just_path = F)

provider_conceptID <- NASA_CMR_finder("10.5067/HLS/HLSS30.002")
