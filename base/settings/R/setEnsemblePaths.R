#' Set paths to input file ensembles using a consistent pattern across sites
#'
#' Propagates a filename pattern into the relevant paths of a multi-site
#' settings. For example if your files are named like "mymet/siteA/scenario1.nc"
#' up through "mymet/siteZ/scenario50.nc",
#' `setEnsemblePaths(settings, n_reps = 50, "mymet/{id}/scenario{n}.nc")` will
#' add them all to your settings in one shot.
#'
#' Operates on one input section (met, poolinitcond, etc) at a time because
#' it's common to have different path conventions for met vs IC.
#'
#' The path template should be a string recognized by `glue::glue()`,
#' with curly braces wrapping any expressions to be interpolated.
#' `{n}` will be replaced with each value of 1:`n_reps`, `{id}` will be
#' replaced with the siteid of each site, and any other variables need to be
#' passed as named arguments in `...`.
#'
#' Note that for consistency, every site in `settings` must contain an
#' element named `inputs$<input_type>` before you call this.
#' If `inputs$<input_type>$path` does not exist it will be created;
#' if it does exist it will be overwritten.
#'
#' @param settings a PEcAn MultiSettings object
#' @param n_reps number of replicates to insert for each path.
#' @param input_type subsection of the `inputs` settings to be edited
#' @param path_template format for the paths to be inserted, as a `glue` string.
#' @param ... additional variables to be interpolated into `path_template`
#' @return updated settings object
#' @examples
#' s <- as.Settings(list(
#'   run = list(
#'     start.date = "TBD",
#'     site = list(),
#'     inputs = list(
#'       met = list(),
#'       poolinitcond = list()
#'     )
#'   )
#' ))
#' m <- createMultiSiteSettings(s, c("a1", "b2"))
#' m1 <- setEnsemblePaths(m, 2)
#' m1$run$site.a1$inputs
#' m2 <- setEnsemblePaths(
#'   m, 2, "poolinitcond",
#'   icdir = "some/long/path",
#'   path_template = "{icdir}/{id}/{n}.nc"
#' )
#' m2$run$site.a1$inputs
#' @export
setEnsemblePaths <- function(
    settings,
    n_reps,
    input_type = c("met", "poolinitcond", "soilinitcond"),
    path_template = "./{id}/{n}.nc",
    ...) {
  if (!is.MultiSettings(settings)) {
    PEcAn.logger::logger.error(
      "Setting ensemble paths is only implemented for MultiSettings objects"
    )
  }
  input_type <- match.arg(input_type)

  papply(
    settings,
    set_site_paths,
    input_type = input_type,
    n = n_reps,
    glue_str = path_template,
    ...
  )
}


# Set pathset at one site,
# inserting site id if path template contains `{id}`
set_site_paths <- function(settings, input_type, ...) {
  siteid <- settings$run$site$id
  paths <- build_pathset(id = siteid, ...)
  settings$run$inputs[[input_type]]$path <- paths

  settings
}


#' Construct paths to a series of similarly-named files
#'
#' Given a template string and a set of variables that includes a replicate ID,
#' creates a list of paths matching the template and returns them in a list
#' ready to insert into a PEcAn `settings` object.
#'
#' The motivating use case is populating the ensemble files (`met$path`,
#' `poolinitcond$path`, etc) inside `settings$run$site.<x>$inputs`
#' when creating multi-site run configurations. It can also be helpful for
#' e.g. series of yearly files, or more generally for constructing any set of
#' paths that both (1) have names differing by one variable, and (2) should be
#' inserted into a PEcAn settings object as one block of `<path>` entries:
#'
#' ```
#' <path>
#'   <path1>path/to/siteA/file.1.ext</path1>
#'   <path2>path/to/siteA/file.2.ext</path2>
#'   [...]
#'   <path[n]>path/to/siteA/file.[n].ext</path[n]>
#' </path>
#' ```
#'
#' @param n vector of replicate ids.
#'  If given as a single numeric value, it will be expanded to `1:n`
#' @param glue_str Specification of how to interpolate variables into the
#'  paths, as a `glue::glue()` input (see examples)
#' @param ... other variables to be interpolated into the path, each with length
#'  either 1 or equal to `n`.
#' @return list of paths the same length as `n`, with names set as `path<n>`
#' @keywords internal
### ^ Internal for now, but OK to export later if that proves useful.
### If/when exporting, remove the \dontrun{} below along with this comment --
###  it's only here bc R checks complain if examples use unexported functions
#' @examples
#' \dontrun{
#' build_pathset(3, "IC/foo/IC_foo_{n}.nc")
#' build_pathset(n = 1:3, id = "2ce800f4", yr = 2003, "ERA5/{id}/{yr}_{n}.clim")
#' }
build_pathset <- function(n, glue_str = "./file_{n}.nc", ...) {
  if (length(n) == 1 && is.numeric(n)) {
    n <- seq_len(n)
  }
  values <- list(n = n, ...)
  glue::glue_data(.x = values, glue_str) |>
    as.list() |>
    stats::setNames(glue::glue("path{n}", n = n))
}
