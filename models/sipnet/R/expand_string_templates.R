#' Replace `@TEMPLATE@` placeholders in a set of lines of text
#'
#' Helper that mostly exists to save (a little) typing when copying string
#' template replacement from write.configs into write_segmented_configs.
#'
#' Replacements are done iteratively in argument order and all apply to the
#' entire text, so earlier-listed values can contain templates for later
#' expansion, e.g.
#' `expand_string_templates(txt, settings, SETUP="mkdir @MYDIR@", MYDIR="out")`
#' will expand "bash -c '@SETUP@'" to "bash -c 'mkdir out'".
#'
#' @param text lines of text (destined for job.sh) in which to replace @strings@
#' @param settings PEcAn settings object to look in for default values
#' @param ... other templates to replace, listed first so they can include
#'   later-expanded templates within themselves
#' @param HOST_SETUP,CDO_SETUP,HOST_TEARDOWN host-specific setup and teardown
#' @param CPRUNCMD,CPOUTCMD,RMOUTDIRCMD,RMRUNDIRCMD commands to manipulate and
#'   clean up the workflow directory after model execution. Not all templates
#'   use these
#' @param SITE_LAT,SITE_LON,START_DATE,END_DATE location and date range being
#'   modeled
#' @param SITE_MET path to clim file
#' @param OUTDIR,RUNDIR PEcAn run and out directories for this invocation
#' @param BINARY,REVISION Sipnet model information
#' @param PREFIX,OVERWRITE,CONFLICT settings specific to SDA workflow
#' @param DELETE.RAW logical: Delete each `sipnet.out` after writing to netcdf?
#'
#' @return Modified text with the listed @TEMPLATE@s replaced
#'
#' @keywords internal
#' @author Chris Black
expand_string_templates <- function(
    text,
    settings,
    ...,
    HOST_SETUP = paste(
      paste(settings$model$prerun, sep = "\n"),
      paste(settings$host$prerun, sep = "\n"),
      "",
      sep = "\n"
    ),
    CDO_SETUP = paste(settings$host$cdosetup, "", sep = "\n"),
    HOST_TEARDOWN = paste(
      paste(settings$model$postrun, sep = "\n"),
      paste(settings$host$postrun, sep = "\n"),
      "",
      sep = "\n"
    ),
  CPRUNCMD = "",
  CPOUTCMD = "",
  RMOUTDIRCMD = "",
  RMRUNDIRCMD = "",
  SITE_LAT = settings$run$site$lat,
  SITE_LON = settings$run$site$lon,
  SITE_MET = settings$run$inputs$met$path,
  OUTDIR = shQuote(settings$host$outdir),
  RUNDIR = shQuote(settings$host$rundir),
  START_DATE = settings$run$start.date,
  END_DATE = settings$run$end.date,
  BINARY = settings$model$binary,
  REVISION = settings$model$revision,
  PREFIX = settings$state.data.assimilation$NC.Prefix,
  OVERWRITE = settings$state.data.assimilation$NC.Overwrite,
  CONFLICT = settings$state.data.assimilation$FullYearNC,
  DELETE.RAW = settings$model$delete.raw
) {

  strings <- list(...) |>
    append(
      c(
        HOST_SETUP = HOST_SETUP,
        CDO_SETUP = CDO_SETUP,
        HOST_TEARDOWN = HOST_TEARDOWN,
        CPRUNCMD = CPRUNCMD,
        CPOUTCMD = CPOUTCMD,
        RMOUTDIRCMD = RMOUTDIRCMD,
        RMRUNDIRCMD = RMRUNDIRCMD,
        SITE_LAT = SITE_LAT,
        SITE_LON = SITE_LON,
        SITE_MET = SITE_MET,
        OUTDIR = OUTDIR,
        RUNDIR = RUNDIR,
        START_DATE = START_DATE, 
        END_DATE = END_DATE,
        BINARY = BINARY,
        REVISION = REVISION,
        PREFIX = PREFIX,
        OVERWRITE = OVERWRITE,
        CONFLICT = CONFLICT,
        DELETE.RAW = DELETE.RAW
      )
    )

  templates <- paste0("@", names(strings), "@")

  for (i in seq_along(strings)) {
  	text <- gsub(templates[[i]], strings[[i]], text)
  }
  text
}
