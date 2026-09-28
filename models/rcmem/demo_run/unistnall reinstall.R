# Uninstall reinstall

# Uninstall and reinstall latest branch from GitHub

# 1. If rCMEM is loaded and in the memory, forget rCMEM
if ("rCMEM" %in% (.packages())){
  detach("package:rCMEM", unload=TRUE) 
}

# 2. If remotes is not already installed, install it
if (! ("remotes" %in% installed.packages())) {
  install.packages("remotes")
}

# 3. Install package from developer branch of GitHub
devtools::install_github("https://github.com/Smithsonian/rCMEM")

# 4. Load version into memory
library(rCMEM)


remotes::install_github("abbylewis/pecan", subdir = "models/rcmem", 
                        ref = "mem_dev")



if ("data.water" %in% (.packages())){
  detach("package:data.water", unload=TRUE) 
}

remotes::install_github("abbylewis/pecan", 
                        subdir = "modules/data.water", 
                        ref = "mem_dev")
