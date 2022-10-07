# Prep internal data for package.  See https://r-pkgs.org/data.html for explanation

#read history files
hist_files <- list.files("models/ed/data-raw", pattern = "^history", full.names = TRUE)
hist_list <- lapply(hist_files, read.csv2)
names(hist_list) <- sub(basename(hist_files), pattern = ".csv", replacement = "")
#split up into separate R objects and dump in global environment
list2env(hist_list, envir = .GlobalEnv)

#read soil file and append to list
soil <- read.csv2("models/ed/data-raw/soil.csv")

#save to sysdata.rda
save(list = c(names(hist_list), "soil"), file = "models/ed/R/sysdata.rda")

