
settings.file = "/Users/paleolab/pecan/models/linkages/inst/linkages.xml"

file.copy(paste0("/Users/paleolab/Linkages/met2model_output_v4.2/",site,"/","climate.txt"),
          paste0("/Users/paleolab/pecan/models/linkages/inst/",site,"/run/ENS-00001/"))

PFTs = c("Acer","betula","carya","castanea dentata","fagus grandifolia","picea","pinus","tsuga canadensis","quercus")

#Harvard Forest  Howland Forest	UNDERC	Billy's Lake	Deming Lake	Minden Bog
#-72.18	-68.73	-89.53	-94.58	-95.17	-82.83
#42.54	45.25	46.22	46.28	47.17	43.61

site = "PMB"
sitelat = 43.61
sitelon = -82.83 
all_spp_params = read.csv("/Users/paleolab/Linkages/spp_matrix.csv")
pick_spp = c("ash","beech","birch","elm","hemlock","maple","oak","pine","tamarack")
PFTs = as.character(all_spp_params[which(all_spp_params$Spp_Name%in%pick_spp),1])
outdir = paste0("/Users/paleolab/Linkages/met2model_output_v4.2/",site)
model2netcdf.LINKAGES(PFTs = PFTs, outdir = outdir, sitelat = sitelat, sitelon = sitelon, start_date=NULL, end_date=NULL,force=FALSE)

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

