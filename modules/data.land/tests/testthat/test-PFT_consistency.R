# Various tests to ensure that selected PFTs are internally-consistent and consistent with the FIA database
# (recreates bugs in issue #775)
# 
# @author Anthony Cohen
# ----------------------------------------------------------------------------------------------------------

betyparms <- PEcAn.DB::get_postgres_envvars(
  host = "localhost",
  dbname = "bety",
  user = "bety",
  password = "bety",
  driver = "Postgres",
  write = FALSE)
fiaparms <- betyparms
fiaparms$dbname <- "fia5data"
if (PEcAn.DB::db.exists(params = betyparms) && PEcAn.DB::db.exists(fiaparms)) {


  test_that("PFTs don't overlap species", {
    
    overlapping.pfts <- PEcAn.settings::read.settings("dup_species.xml")		#settings list
    #expect_output(fia.to.psscss(overlapping.pfts), "ERROR \\[.*\\] : There are [0123456789]+ spcd entries that are duplicated. Please remove overlapping PFTs.")	
    expect_error(fia.to.psscss(overlapping.pfts))	
    
  })
  
  
  test_that("User is warned if PFTs have extra species not suggested by FIA", {
    
    extra.pft <- PEcAn.settings::read.settings("wrong_pft.xml")
    expect_error(fia.to.psscss(extra.pft))  
  })
  
  
  test_that("PFTs encompass all species suggested by FIA", {
    
    insufficient.pft <- PEcAn.settings::read.settings("wrong_pft.xml")
    expect_error(fia.to.psscss(insufficient.pft))
  })
  
  # Regex notes: 1) the timestamp that goes into the console is not read in the regex!	  
  #              2) [:digit:] does not work without changing locale. Neither does \d
  #              3) logger does not interpret whitespace requests like \n
}
