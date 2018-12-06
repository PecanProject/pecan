library(PEcAn.ED2)

outdir <- "~/sda-hackathon/outputs"
runid_vec <- readLines(file.path(outdir, "run", "runs.txt"))
runid <- runid_vec[1]

settings_file <- file.path(outdir, "pecan.CONFIGS.xml")
settings <- PEcAn.settings::read.settings(settings_file)

start.time <- as.POSIXlt("2004-06-07", tz = "UTC")
stop.time <- as.POSIXlt("2004-06-10", tz = "UTC")

forecast <- read_restart.ED2(outdir = outdir,
                             runid = runid, 
                             stop.time = stop.time,
                             settings = settings,
                             var.names = "AGB",
                             params = NULL)

npft <- length(forecast)

set.seed(666)
new.state <- rnorm(npft, forecast, 0.001)
names(new.state) <- names(forecast)

write_restart <- write_restart.ED2(outdir = outdir,
                                   runid = runid,
                                   start.time = start.time,
                                   stop.time = stop.time,
                                   settings = settings,
                                   new.state = new.state,
                                   new.params = NULL,
                                   inputs)
