
test_that("AllomAve writes raw outputs to specified path", {
	outdir <- tempfile("allomAve_test")
	withr::local_file(outdir) # deletes outdir when test ends
	dir.create(outdir, recursive = TRUE)

	pfts <- list(FAGR = data.frame(spcd = 531, acronym = "FAGR"))
	allom_stats <- AllomAve(
		pfts,
		components = 6,
		outdir = outdir,
		ngibbs = 5,
		nchain = 2)

	expect_true(file.exists(file.path(outdir, "Allom.FAGR.6.Rdata")))
	expect_true(file.exists(file.path(outdir, "Allom.FAGR.6.MCMC.pdf")))

})

test_that("AllomAve writes to cwd by default", {
	outdir <- tempfile("allomAve_test_cwd")
	withr::local_file(outdir) # deletes outdir when test ends
	dir.create(outdir, recursive = TRUE)
	withr::local_dir(outdir) # sets working dir until test ends

	pfts <- list(FAGR = data.frame(spcd = 531, acronym = "FAGR"))
	allom_stats <- AllomAve(
		pfts,
		components = 18,
		ngibbs = 5,
		nchain = 2)

	expect_true(file.exists(file.path(outdir, "Allom.FAGR.18.Rdata")))
	expect_true(file.exists(file.path(outdir, "Allom.FAGR.18.MCMC.pdf")))
})
