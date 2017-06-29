dontrun <- function() { ## Added by Alexey Shiklomanov so this doesn't run and break the build

# This isn't necessarily a fully working settings object. Enough to get the idea though. 
# Note it has a $run block with settings that will be shared across all sites

template = Settings(
  list(info = structure(list(notes = NULL, userid = "1000000005", 
    username = "Ryan Kelly", date = "2016/07/13 13:23:46 -0400"), .Names = c("notes", 
"userid", "username", "date")), database = structure(list(bety = structure(list(
    user = "bety", password = "bety", host = "psql-pecan.bu.edu", 
    dbname = "bety", driver = "PostgreSQL", write = "TRUE"), .Names = c("user", 
"password", "host", "dbname", "driver", "write")), fia = structure(list(
    user = "bety", password = "bety", host = "psql-pecan.bu.edu", 
    dbname = "fia5", driver = "PostgreSQL", write = "true"), .Names = c("user", 
"password", "host", "dbname", "driver", "write"))), .Names = c("bety", 
"fia")), pfts = structure(list(pft = structure(list(comment = NULL, 
    name = "temperate.Evergreen_Hardwood", constants = structure(list(
        num = "1"), .Names = "num")), .Names = c("comment", "name", 
"constants")), pft = structure(list(name = "temperate.Hydric", 
    constants = structure(list(num = "2"), .Names = "num")), .Names = c("name", 
"constants"))), .Names = c("pft", "pft")), meta.analysis = structure(list(
    iter = "3000", random.effects = "FALSE", update = "AUTO", 
    threshold = "1.2"), .Names = c("iter", "random.effects", 
"update", "threshold")), ensemble = structure(list(size = "1", 
    variable = "NPP"), .Names = c("size", "variable")), model = structure(list(
    id = "2000000005", edin = "/home/rykelly/pecan/RK_files/ED2IN/ED2IN.rgit.mandifore_04", 
    config.header = structure(list(radiation = structure(list(
        lai_min = "0.01"), .Names = "lai_min"), ed_misc = structure(list(
        output_month = "12"), .Names = "output_month")), .Names = c("radiation", 
    "ed_misc")), phenol.scheme = "0", prerun = "module load hdf5/1.8.11", 
    binary = "/usr2/postdoc/rykelly/ED2/ED/build/ed_2.1-opt"), .Names = c("id", 
"edin", "config.header", "phenol.scheme", "prerun", "binary")), 
    host = structure(list(name = "geo.bu.edu", user = "rykelly", 
        folder = "/projectnb/dietzelab/pecan.data/output/rykelly", 
        qsub = "qsub -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash", 
        qsub.jobid = "Your job ([0-9]+) .*", qstat = "qstat -j @JOBID@ || echo DONE", 
        prerun = "module load udunits R/R-3.0.0_gnu-4.4.6", dbfiles = "/projectnb/dietzelab/pecan.data/input", 
        modellauncher = structure(list(binary = "/usr2/postdoc/rykelly/pecan/utils/modellauncher/modellauncher", 
            qsub.extra = "-pe omp 20"), .Names = c("binary", 
        "qsub.extra"))), .Names = c("name", "user", "folder", 
    "qsub", "qsub.jobid", "qstat", "prerun", "dbfiles", "modellauncher"
    )), run = structure(list(inputs = structure(list(met = structure(list(
        source = "NARR", output = "ED2"), .Names = c("source", 
    "output")), lu = structure(list(id = "294", path = "/projectnb/dietzelab/EDI/ed_inputs/glu/"), .Names = c("id", 
    "path")), soil = structure(list(id = "297", path = "/projectnb/dietzelab/EDI/faoOLD/FAO_"), .Names = c("id", 
    "path")), thsum = structure(list(id = "295", path = "/projectnb/dietzelab/EDI/ed_inputs/"), .Names = c("id", 
    "path")), veg = structure(list(id = "296", path = "/projectnb/dietzelab/EDI/oge2OLD/OGE2_"), .Names = c("id", 
    "path")), pss = structure(list(source = "FIA"), .Names = "source")), .Names = c("met", 
    "lu", "soil", "thsum", "veg", "pss")), start.date = "2004/01/01", 
        end.date = "2004/01/31"), .Names = c("inputs", "start.date", 
    "end.date")))
)


sitegroupId <- 1000000002
startDate = "2000/01/01"
endDate = "2015/12/31"
nSite <- 10
outDir = '~/multisite_setup_test'


template <- setDates(template, startDate = startDate, endDate = endDate)
template <- setOutDir(template, outDir)

multiRunSettings <- createSitegroupMultiSettings(template, sitegroupId = sitegroupId, nSite=nSite)

dir.create(outDir, showWarnings=F)
write.settings(multiRunSettings, outputfile="pecan.xml")

} # dontrun
