.get.test.settings = function() {
  settings <- NULL
  try({
    if(PEcAn.remote::fqdn() == "pecan2.bu.edu") {
      settings <- read.settings("testinput.pecan2.bu.edu.xml")
    } else {
      settings <- read.settings("testinput.xml")
    }
  }, 
  silent=T)
  
  if(is.null(settings)) {
    skip("Can't get a valid test Settings right now. Skipping test. ")
  }
  return(settings)
}
