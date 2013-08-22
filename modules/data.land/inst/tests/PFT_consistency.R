# Various tests to ensure that selected PFTs are internally-consistent and consistent with the FIA database
# (recreates bugs in issue #775)
# 
# @author Anthony Cohen
# ----------------------------------------------------------------------------------------------------------
require(PEcAn.utils)

if(!db.exists())
{
	stop("bety database is not available. Stopping.")
}

context("PFTs")



test_that("PFTs don't overlap species", {
			
			overlapping.pfts <- read.settings("modules/data.land/inst/Tests/dup_species.xml")		#settings list
			#expect_output(fia.to.psscss(overlapping.pfts), "ERROR \\[.*\\] : There are [0123456789]+ spcd entries that are duplicated. Please remove overlapping PFTs.")	
			expect_output(fia.to.psscss(overlapping.pfts), "ERROR \\[.*\\] : The following species are found in multiple PFTs:.*Please remove overlapping PFTs.")	
			
})


test_that("User is warned if PFTs have extra species not suggested by FIA", {

			extra.pft <- read.settings("modules/data.land/inst/Tests/wrong_pft.xml")  
			expect_output(fia.to.psscss(extra.pft), ".*WARN  \\[.*\\] : The selected PFTs contain .+ species for which the FIA database contains no data at.*These will be populated with zero values in the output.")  
})


test_that("PFTs encompass all species suggested by FIA", {
			
			insufficient.pft <- read.settings("modules/data.land/inst/Tests/wrong_pft.xml")
			expect_output(fia.to.psscss(insufficient.pft), ".*ERROR \\[.*\\] : The FIA database expects .+ species .+ are not described by the selected PFTs.*Please select additional pfts.")
})

# Regex notes: 1) the timestamp that goes into the console is not read in the regex!	  
#              2) [:digit:] does not work without changing locale. Neither does \d
#              3) logger does not interpret whitespace requests like \n
