
site = "PDL"
settings <- read.settings(paste0("/Users/paleolab/pecan/models/linkages/inst/pecan_",site,".xml"))
file.copy(paste0("/Users/paleolab/Linkages/met2model_output/",site,"/","test_text1.txt"),
          paste0("/Users/paleolab/pecan/pecan/PEcAn_LINKAGES_",site,"/run/ENS-00001/"))



