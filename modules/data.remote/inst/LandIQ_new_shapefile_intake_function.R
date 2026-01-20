# Script for joining new years' data to existing shapefiles
# First function shapefile_grab is just for pulling shapefile into R before processing
# Second function get_CARB_data is for processing shapefile data so it can be joined to PEcAn-ready dataframe

####---- Libraries ----####
install.packages("librarian")
library(librarian)
#remotes::install_github("rspatial/terra")
librarian::shelf(terra, sf, tidyverse, stringr)


####---- Function for loading shapefiles into R: ----####
#' @param base_dir = folder where data are located (character)
#' @param year = year data were collected (numeric)
#' @param sf = TRUE -> open as simple feature or FALSE -> open as spatvector (TRUE/FALSE)
shapefile_grab <- function(base_dir, year, sf) {
  filelist <- list.files(base_dir, pattern = as.character(year))  # open files for specified year
  shpfile <- list.files(paste0(base_dir, "/", filelist), pattern = ".shp")  # shapefiles
  file <- paste0(base_dir, "/", filelist, "/", shpfile[1])  # make full filepath
  if (sf == TRUE){
    st_read(file)  # if TRUE - read in as sf (using sf library)
  } else {
    vect(file)  # if FALSE - read in as spatvector (using terra library)
  }
}

### Wrapper for CARB shapefile dataframe maker functions:
#' @param crops = crop sf object (sf object, shapefile)
#' @param crs = desired crs (numeric, I have been using 3857)
#' @param year = year of data (numeric; YYYY)
get_CARB_data <- function(crops, crs, year){
  # transform crs, make valid, and find centroids:
  centr <- function(shp, crs, year){
    crops <- st_transform(shp, crs) # change crs 
    crops <- st_make_valid(crops) # make valid
    cents <- st_centroid(crops)  # find centroid
    cents <- st_transform(cents, crs)  # make same projection
    centpts <- st_coordinates(cents)  # get coordinates from centroids
    crops$centx <- centpts[,'X']  # add columns for coordinates to shapefile
    crops$centy <- centpts[,'Y']
    crops$year <- year  # add a column for data year
    crops <- st_zm(crops, drop = T, what = "ZM") # correct the extra Z dimension in geometry
    return(crops)
  }
  crop_new <- centr(crops, crs, year)
  
  # grab columns for data frame conversion:
  col_grab <- function(shp){
    id <- grep("^Unique", names(shp))    # UniqueID from LandIQ
    year <- grep("year", names(shp))     # year of data
    lon <- grep("^centx", names(shp))    # centroid lon
    lat <- grep("^centy", names(shp))    # centroid lat
    mult <- grep("^MULT", names(shp))    # multiuse code
    class <- grep("^CLASS", names(shp))  # class code
    sub <- grep("^SUB", names(shp))      # subclass number
    spec <- grep("^SPEC", names(shp))    # special condition code
    sen <- grep("^SEN", names(shp))      # senescing crop
    emer <- grep("^EMER", names(shp))    # emerging crop
    irst <- grep("PA", names(shp))       # irrigation status (from IRR_TYP#PA)
    irty <- grep("PB", names(shp))       # irrigation type (from IRR_TYP#PB)
    pcnt <- grep("^PCN", names(shp))     # percent cover
    adoy <- grep("^ADOY", names(shp))    # adjusted day of year for crops
    yr_pl <- grep("^YR", names(shp))     # year planted
    hy_reg <- grep("^HYD", names(shp))   # hydro region
    reg <- grep("^REG", names(shp))      # region
    cty <- grep("COUNTY", names(shp))    # county
    
    # combine
    cols <- c(id, year, lon, lat,
              mult, class, sub, spec, sen, emer,
              irst, irty, pcnt, adoy, yr_pl,
              hy_reg, reg, cty)
    crops <- shp[,cols]
    return(crops)
  }
  crop_grab <- col_grab(crop_new)
  
  # build data frame from shapefile object:
  crop_clean <- function(shp){
    df <- st_drop_geometry(shp)
    
    # get rid of asterix values, replace 00 percent codes to 100, make number cols numeric:
    df_clean <- df %>% 
      mutate(across(everything(), na_if, "**")) %>%
      mutate(across(everything(), na_if, "*")) %>%
      mutate(across(everything(), ~replace(., . == "00", "100"))) %>%
      mutate(across(everything(), as.character))
    
    # make longer:
    # prepare columns for sorting into seasons:
    renamer <- function(df){
      numb <- str_extract(names(df), "[0-9]")
      char <- str_extract_all(names(df), "[:alpha:]+", simplify = TRUE)
      ch <- vector()
      for (i in 1:nrow(char)){
        ch[i] <- str_c(char[i,], collapse = "")
      }
      newnames <- str_remove(paste(numb, ch, sep = ""), "NA")
      colnames(df) <- newnames
      return(df)
    }
    df_cl <- renamer(df_clean)
    
    # pivoting:
    df_piv <- df_cl %>% 
      pivot_longer(-c(grep("^[a-zA-Z]", names(df_cl))), names_to = c("name"), values_to = "value") %>%
      mutate(type = str_extract(name, "[A-Z]+")) %>%
      mutate(season = str_extract(name, "[0-9]")) %>%
      select(-name) %>% group_by(UniqueID) %>%
      pivot_wider(names_from = type,
                  values_from = value) %>% ungroup() %>%
      mutate(across(c("UniqueID", "year", "SUBCLASS", "PCNT", "ADOY", "season"), as.numeric))
  }
  crops_out <- crop_clean(crop_grab)
  return(crops_out)
}

# join output to combined dataframe (previous year data) with rbind

# example with 2023 provisional data:
#setwd("/projectnb/dietzelab/malmborg/CARB")
crs = 3857
year = 2023
crops <- shapefile_grab("LandIQ_shps/", year, sf = TRUE)
new_crops <- get_CARB_data(crops, crs, year)
