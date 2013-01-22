result <- read.csv(system.file("result.csv", package = "PEcAn.BIOCRO"))

outdir <- "~/R-dev/pecan/models/biocro/inst/"
#test_that{"we can create a netcdf file in PEcAn default format",
model2netcdf.BIOCRO(outdir=outdir)
ensemble.output <- read.ensemble.output(1,outdir = outdir, 
                                                  start.year=2004,
                                                  end.year=2004,
                                                  variables="Stem")          
}