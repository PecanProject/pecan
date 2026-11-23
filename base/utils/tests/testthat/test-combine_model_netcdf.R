# Read and close each file immediately, including when an assertion fails.
read_merge_variable <- function(path, variable, collapse_degen = TRUE) {
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  values <- ncdf4::ncvar_get(nc, variable, collapse_degen = collapse_degen)
  if (collapse_degen) as.vector(values) else values
}

test_that("single-site merge preserves ensemble and time coordinates", {
  skip_if_not_installed("ncdf4")
  wd <- withr::local_tempdir()
  output <- file.path(wd, "merged")
  dir.create(output)

  # Unequal dimension lengths and distinct values expose transposed writes.
  # Include singleton dimensions and missing values as well as two variables
  # to exercise both the initial creation and ncvar_add write paths.
  for (ntime in c(1, 4)) {
    expected <- list(
      AGB = outer(seq_len(ntime), 1:2, function(t, e) 100 * e + t),
      GPP = outer(seq_len(ntime), 1:2, function(t, e) -10 * e - t)
    )
    expected$GPP[ntime, 2] <- NA_real_
    for (member in 1:2) {
      member_dir <- file.path(wd, sprintf("ENS-%05d-101", member))
      dir.create(member_dir, showWarnings = FALSE)
      dims <- list(
        ncdf4::ncdim_def("lon", "degrees_east", -90),
        ncdf4::ncdim_def("lat", "degrees_north", 40),
        ncdf4::ncdim_def("time", "days since 2019-01-01", seq_len(ntime) - 1)
      )
      vars <- lapply(names(expected), function(name) {
        ncdf4::ncvar_def(name, "kg m-2", dims, missval = -9999)
      })
      nc <- ncdf4::nc_create(file.path(member_dir, "2019.nc"), vars)
      for (name in names(expected)) {
        ncdf4::ncvar_put(nc, name, expected[[name]][, member])
      }
      ncdf4::nc_close(nc)
    }
    for (nmember in 1:2) {
      path <- nc_merge_single_site(wd, output, nmember, 101, 2019)
      for (name in names(expected)) {
        actual <- read_merge_variable(path, name, collapse_degen = FALSE)
        expect_equal(dim(actual), c(1L, nmember, ntime))
        for (member in seq_len(nmember)) {
          expect_equal(as.vector(actual[1, member, ]), expected[[name]][, member], tolerance = 0)
        }
      }
      expect_equal(read_merge_variable(path, "site_id"), 101)
      expect_equal(read_merge_variable(path, "latitude"), 40)
      expect_equal(read_merge_variable(path, "longitude"), -90)
      expect_equal(read_merge_variable(path, "time"), seq_len(ntime) - 1)
    }
  }
})

test_that("annual merge preserves fixture values at each site, member and time", {
  skip_if_not_installed("ncdf4")
  skip_if_not_installed("doSNOW")
  skip_if_not(nzchar(Sys.which("cdo")), "cdo not available")
  src <- test_path("data/ensemble_fixtures")
  wd <- withr::local_tempdir()
  site_ids <- c(101, 102)
  fixture_sites <- c("e968e9c8f8574cb2", "ebb783e86d2ac6fb")
  for (site in seq_along(site_ids)) {
    for (member in 1:2) {
      destination <- file.path(wd, sprintf("ENS-%05d-%s", member, site_ids[site]))
      dir.create(destination)
      source <- file.path(src, sprintf("ENS-%05d-%s", member, fixture_sites[site]))
      expect_true(all(file.copy(list.files(source, pattern = "[.]nc$", full.names = TRUE), destination)))
    }
  }
  for (nmember in 1:2) {
    output <- file.path(wd, paste0("merged", nmember))
    paths <- nc_merge_all_sites_by_year(
      wd, output, nmember, site_ids, "2019-01-01", "2020-12-31", cores = nmember
    )
    expect_equal(paths, file.path(output, c("2019.nc", "2020.nc")))
    for (path in paths) {
      nc <- ncdf4::nc_open(path)
      expect_equal(vapply(nc$var$AGB$dim, function(d) d$name, ""), c("site", "ensemble", "time"))
      ncdf4::nc_close(nc)
      ids <- read_merge_variable(path, "site_id")
      expect_setequal(ids, site_ids)
      for (variable in c("AGB", "GPP")) {
        actual <- read_merge_variable(path, variable, collapse_degen = FALSE)
        for (site in site_ids) {
          for (member in seq_len(nmember)) {
            source <- file.path(wd, sprintf("ENS-%05d-%s", member, site), basename(path))
            expect_equal(actual[match(site, ids), member, ], read_merge_variable(source, variable), tolerance = 0)
          }
        }
      }
    }
    # Existing annual files are reused without modifying their contents.
    before <- tools::md5sum(paths)
    expect_equal(nc_merge_all_sites_by_year(
      wd, output, nmember, site_ids, "2019-01-01", "2020-12-31", cores = nmember
    ), paths)
    expect_identical(tools::md5sum(paths), before)
  }
})
