extdata <- system.file("extdata", package = "PEcAn.utils")
outdir <- "/tmp/out/"


read.output(run.id = "SAmedian", outdir = outdir, start.year=1999, end.year=2000,variables="NPP")
settings$model$name


file.copy(dir(extdata, pattern = "testENS-000[1-4].nc", full.names = TRUE), outdir)