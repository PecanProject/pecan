extdata <- system.file("extdata", package = "PEcAn.utils")
outdir <- "/tmp/out/"
file.copy(dir(extdata, pattern = "testENS-000[1-4].nc", full.names = TRUE), outdir)

