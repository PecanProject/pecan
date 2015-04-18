
settings.file = "/Users/paleolab/pecan/models/linkages/inst/linkages.xml"

site = "PHA"
file.copy(paste0("/Users/paleolab/Linkages/met2model_output_v4.2/",site,"/","climate.txt"),
          paste0("/Users/paleolab/pecan/models/linkages/inst/",site,"/run/ENS-00001/"))

PFTs = c("Acer","betula","carya","castanea dentata","fagus grandifolia","picea","pinus","tsuga canadensis","quercus")








met2model.LINKAGES(in.path = "/Users/paleolab/Linkages/phase1a_met_drivers_v4.2/PUN",
                   in.prefix = "PUN", 
                   outfolder = "/Users/paleolab/linkages/met2model_output_v4.2/PUN/",
                   start_date = "0850/01/01",
                   end_date = "2010/12/31", 
                   overwrite=FALSE,verbose=FALSE)
met2model.LINKAGES(in.path = "/Users/paleolab/Linkages/phase1a_met_drivers_v4.2/PMB",
                   in.prefix = "PMB", 
                   outfolder = "/Users/paleolab/linkages/met2model_output_v4.2/PMB",
                   start_date = "0850/01/01",
                   end_date = "2010/12/31", 
                   overwrite=FALSE,verbose=FALSE)
met2model.LINKAGES(in.path = "/Users/paleolab/Linkages/phase1a_met_drivers_v4.2/PHA",
                   in.prefix = "PHA", 
                   outfolder = "/Users/paleolab/linkages/met2model_output_v4.2/PHA",
                   start_date = "0850/01/01",
                   end_date = "2010/12/31", 
                   overwrite=FALSE,verbose=FALSE)
met2model.LINKAGES(in.path = "/Users/paleolab/Linkages/phase1a_met_drivers_v4.2/PHO",
                   in.prefix = "PHO", 
                   outfolder = "/Users/paleolab/linkages/met2model_output_v4.2/PHO",
                   start_date = "0850/01/01",
                   end_date = "2010/12/31", 
                   overwrite=FALSE,verbose=FALSE)


write.config.LINKAGES(settings = settings, run.id = run.id)

