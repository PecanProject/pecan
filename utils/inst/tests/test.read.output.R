extdata <- system.file("extdata", package = "PEcAn.utils")
outdir <- tempdir()


read.output(run.id = "SAmedian", outdir = outdir, start.year=1999, end.year=2000,variables="NPP")


file.copy(dir(extdata, pattern = "testENS-000[1-4].nc", full.names = TRUE), outdir)