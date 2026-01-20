test_that("merge_by_time works on real-ish fixtures", {

    #--- Setup ------------------------------------------
    library(ncdf4)
    skip_if_not_installed("ncdf4")
    skip_if_not(nzchar(Sys.which("cdo")), "cdo not available")

    src <- testthat::test_path("data/ensemble_fixtures") # PEcAn output fixtures
    wd <- withr::local_tempdir()

    files_copied <- file.copy(list.files(src, full.names = TRUE), wd, recursive = TRUE)
    expect_true(all(files_copied))

    # Set fixed values describing test files
    ens_num  <- 2
    site.ids <- c("e968e9c8f8574cb2", "ebb783e86d2ac6fb")
    start.date <- "2019-01-01"
    end.date   <- "2020-12-31"

    # --- run function merge funciton ----------------------
    nc_merge_all_sites_by_year(
        model.outdir = src,
        nc.outdir    = wd,
        ens.num      = ens_num,
        site.ids     = site.ids,
        start.date   = "2019-01-01",
        end.date     = "2020-12-31",
        cores        = 2
    )

    # Assert shape + contents
    f <- ncdf4::nc_open(file.path(wd, "2019.nc")); withr::defer(ncdf4::nc_close(f))
    expect_true(all(c("time", "site", "ensemble") %in% names(f$dim)))
    sid <- ncdf4::ncvar_get(f, "site_id")
    expect_equal(length(sid), f$dim$site$len)
    expect_setequal(as.character(sid), unique(site.ids))

    # Compare GPP for sites 1 and 2 in merged file to source file
    # Site 1
      gpp_merged <- ncdf4::ncvar_get(f, "GPP")
      src_first <- file.path(
          src,
          "ENS-00001-e968e9c8f8574cb2",
          "2019.nc"
      )
      f0 <- ncdf4::nc_open(src_first)
      withr::defer(ncdf4::nc_close(f0))
      gpp_src <- ncdf4::ncvar_get(f0, "GPP")

      # take the first N time points that exist in both files
      expect_equal(
          gpp_merged,
          gpp_src,
          tolerance = 1e-6
      )
    
      # Site 2
      gpp_merged_s2 <- ncdf4::ncvar_get(f, "GPP")
      src_s2 <- file.path(
          src,
          "ENS-00001-ebb783e86d2ac6fb",
          "2019.nc"
      )
      f2 <- ncdf4::nc_open(src_s2)
      withr::defer(ncdf4::nc_close(f2))
      gpp_src_s2 <- ncdf4::ncvar_get(f2, "GPP")

      expect_equal(
          gpp_merged_s2,
          gpp_src_s2,
          tolerance = 1e-6
      )

      # Ensure that re-running does not change contents
      gpp_before <- ncdf4::ncvar_get(f, "GPP")
      ncdf4::nc_close(f)
      nc_merge_all_sites_by_year(
        model.outdir = src,
        nc.outdir    = wd,
        ens.num      = ens_num,
        site.ids     = site.ids,
        start.date   = "2019-01-01",
        end.date     = "2020-12-31",
        cores        = 2
      )
      f <- ncdf4::nc_open(file.path(wd, "2019.nc"))
      withr::defer(ncdf4::nc_close(f))
      gpp_after <- ncdf4::ncvar_get(f, "GPP")
      expect_equal(gpp_after, gpp_before, tolerance = 1e-6)
})