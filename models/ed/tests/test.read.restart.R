library(PEcAn.ED2)

outdir <- "~/sda-hackathon/outputs"
runid_vec <- readLines(file.path(outdir, "run", "runs.txt"))

settings_file <- file.path(outdir, "pecan.CONFIGS.xml")
settings <- PEcAn.settings::read.settings(settings_file)

rred <- function(runid) {
    fc <- read.restart.ED2(outdir = outdir, 
                           runid = runid,
                           stop.time = NULL,
                           settings = settings,
                           var.names = "AGB",
                           params = NULL)
    return(fc)
}

forecast_list <- lapply(runid_vec, rred)
names(forecast_list) <- runid_vec

print(forecast_list)
