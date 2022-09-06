## code to prepare `pftmapping.csv` dataset goes here
pftmapping <- read.csv2("./models/ed/data-raw/pftmapping.csv")
usethis::with_project(
  "./models/ed/",
  usethis::use_data(pftmapping, overwrite = TRUE)
)
