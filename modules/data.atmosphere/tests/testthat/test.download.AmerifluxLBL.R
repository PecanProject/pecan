
test_that("download respects overwrite argument", {
	outdir <- withr::local_tempdir()
	zippath <- file.path(outdir, "AMF_US-Akn_BASE-BADM_6-5.zip")
	csvpath <- sub("-BADM(.*).zip", "_HH\\1.csv", zippath)
	
	# Mock out amerifluxr functions to test our code without network calls
	local_mocked_bindings(
		amf_download_base = \(...) {
			tmp_csv <- basename(csvpath)
			withr::with_tempdir({
				writeLines(
					c(  "# fake file",
						"#",
						"TIMESTAMP_START,TIMESTAMP_END",
						"201101010000,201101010030",
						"201112310000,201112310030"),
					tmp_csv)
				zip(zippath, tmp_csv, flags = "-qr0X")})
			zippath
		},
		amf_var_info = \(...) data.frame(
			Site_ID = "US-Akn",
			BASE_Version = "6-5"),
		.package = "amerifluxr"
	)
	
	# wrapper, just to skip retyping args
	dl_akn <- function(...) download.AmerifluxLBL(
		site = "US-Akn",
		outfolder = outdir,
		start_date = "2011-01-01",
		end_date = "2011-10-01",
		...)

	# Case 0: new download
	expect_false(file.exists(zippath))
	expect_false(file.exists(csvpath))
	dl_akn()
	expect_true(file.exists(zippath))
	expect_true(file.exists(csvpath))
	

	# Case 1: reuse existing download
	ziptime <- file.mtime(zippath)
	csvtime <- file.mtime(csvpath)
	expect_log(
		dl_akn(overwrite = FALSE),
		"skipping download.*skipping extraction")
	expect_equal(file.mtime(zippath), ziptime)
	expect_equal(file.mtime(csvpath), csvtime)

	# Case 2: overwrite existing download
	dl_akn(overwrite = TRUE)
	expect_gt(file.mtime(zippath), ziptime)
	expect_gt(file.mtime(csvpath), csvtime)

	# Case 3: Freshen csv without clobbering zip
	file.remove(csvpath)
	ziptime <- file.mtime(zippath)
	expect_log(dl_akn(overwrite = FALSE), "skipping download")
	expect_true(file.exists(csvpath))
	expect_equal(file.mtime(zippath), ziptime)

	# Case 4: Re-download zip without clobbering CSV
	# (Note: I'm not sure this is desirable! For consistency it may be better
	#	to overwrite the CSV so we know it matches the zip file.
	# If you change the behavior, go ahead and update this test to match.)
	file.remove(zippath)
	csvtime <- file.mtime(csvpath)
	expect_log(
		dl_akn(overwrite = FALSE),
		"skipping extraction")
	expect_true(file.exists(zippath))
	expect_equal(file.mtime(csvpath), csvtime)
})
