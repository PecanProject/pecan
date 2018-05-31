
#AbbreviatedWorkflow_Sipnet
#Variables :
library(PEcAn.all)
library(PEcAn.utils)
library(RCurl)
variable_S <- c("GPP ", "NPP", "TotalResp","AutoResp", "HeteroResp", "SoilResp", "NEE", "QLE", "leaf_carbon_content", "GWBI", "TotSoilCarb", "course_root_carbon_content", "fine_root_carbon_content", "litter_carbon_content", "Transp", "TotLivBiom", "LAI", "AGB", "SoilMoist", "SoilMoistFracSWE", "AbvGrndWood")

settings <- PEcAn.settings::read.settings("pecan.CHECKED.xml")

for (i in 1:seq_along(variable_S)) {
 
  settings$sensitivity.analysis$variable_S <- variable_S
  print(settings$sensitivity.analysis$variable)
  
  # Get results of model runs
  
    runModule.get.results(settings)
    
  # Run sensitivity analysis and variance decomposition on model output

    runModule.run.sensitivity.analysis(settings)


  
}

