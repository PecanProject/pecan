.get.test.settings <- function(outdir = NULL) {
  settings <- NULL
  try({
    if (PEcAn.remote::fqdn() == "pecan2.bu.edu") {
      settings <- read.settings("data/testinput.pecan2.bu.edu.xml")
    } else {
      settings <- read.settings("data/testinput.xml")
    }
  },
  silent = TRUE)

  # NB environment variables override values in XML here!
  # This is opposite of usual PEcAn rule that XML values always win,
  # but useful here because it allows testing on systems where we
  # don't know the database configuration in advance
  settings$database$bety <- do.call(
    PEcAn.DB::get_postgres_envvars,
    settings$database$bety)
 
  if (is.null(settings)) {
    skip("Can't get a valid test Settings right now. Skipping test. ")
  }
  if (!is.null(outdir)) {
    settings$outdir <- outdir
  }
  return(settings)
}
