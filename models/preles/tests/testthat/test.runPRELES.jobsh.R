## Test runPRELES.jobs.sh

require(PEcAn.all)

outdir <- file.path(tempdir(), "preles")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)


file.copy(from = "data/result.RData", to = outdir)

settings <- PEcAn.settings::read.settings("data/pecan.preles.xml")

start.date<-settings$run$start.date
end.date<-settings$run$end.date



runPRELES.jobsh.R(met.file,outdir,priors,start.date,end.date)
