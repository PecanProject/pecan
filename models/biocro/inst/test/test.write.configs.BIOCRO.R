context("testing write configs")

test_that("write.configs.BIOCRO produces expected output",{
  settings <- PEcAn.utils::read.settings(system.file("pecan.biocro.xml",
                                                     package = "PEcAn.BIOCRO"))
  PEcAn.utils::run.write.configs("BIOCRO")
#  config <- file.path(settings$outdir, settings$workflow$id, "data.xml")
#  xmlParse(config)

}
