## code to prepare `pftmapping.csv` dataset goes here
pftmapping <- read.csv2("C:/Users/stevdher/OneDrive - UGent/Documenten/postdoc/research/LBNL/GitHub/inventory/pecan/models/ed/data-raw/pftmapping.csv")
usethis::with_project(
  "C:/Users/stevdher/OneDrive - UGent/Documenten/postdoc/research/LBNL/GitHub/inventory/pecan/models/ed/",
  usethis::use_data(pftmapping, overwrite = TRUE)
)
