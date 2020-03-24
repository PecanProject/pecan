context("met2model")

outfolder <- tempfile()
setup(dir.create(outfolder, showWarnings = FALSE))
teardown(unlink(outfolder, recursive = TRUE))

test_that("Met conversion runs without error", {
  nc_path <- system.file("test-data", "CRUNCEP.2000.nc",
                         package = "PEcAn.utils")
  in.path <- dirname(nc_path)
  in.prefix <- "CRUNCEP"
  start_date <- "2000-01-01"
  end_date <- "2000-12-31"
  result <- met2model.LINKAGES(in.path, in.prefix, outfolder, start_date, end_date)
  expect_s3_class(result, "data.frame")
  expect_true(file.exists(result[["file"]][[1]]))
})

if(FALSE){
  
  start.year = 850
  end.year = 2010
  site = "PUN"
  
  in.path = paste0("/Users/paleolab/Linkages/phase1a_met_drivers_v4.1/",site,"/")
  outfolder = paste0("/Users/paleolab/Linkages/met2model_output/",site,"/")
  
  met2model.LINKAGES(site = site, in.path = in.path, outfolder = outfolder, start.year = start.year, end.year = end.year)
  
}
