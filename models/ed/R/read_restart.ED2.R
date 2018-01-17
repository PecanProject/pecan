#' @title State data assimilation read-restart for ED2
#'
#' @author Alexey Shiklomanov
#' @inheritParams PEcAn.ModelName::read_restart.ModelName
#' @examples
#' \dontrun{
#'   outdir <- "~/sda-hackathon/outputs"
#'   runid <- "99000000020"
#'   settings_file <- "outputs/pecan.CONFIGS.xml"
#'   settings <- PEcAn.settings::read.settings(settings_file)
#'   forecast <- read_restart.ED2(...)
#' }
#' 
#' @export
read_restart.ED2 <- function(outdir, 
                             runid,
                             stop.time,
                             settings, 
                             var.names, 
                             params) {

    name_separator <- "."

    rundir <- settings$host$rundir
    mod_outdir <- settings$host$outdir

    confxml <- get_configxml.ED2(rundir, runid)

    histfile <- get_restartfile.ED2(mod_outdir, runid, stop.time)
    if (is.null(histfile)) {
      PEcAn.logger::logger.severe("Failed to find ED2 history restart file.")
    }

    nc <- ncdf4::nc_open(histfile)
    on.exit(ncdf4::nc_close(nc))

    # Identify PFTs
    # This assumes that PFT order is the same between pecan.xml and ED's 
    # config.xml.
    # A better solution would set the PFT numbers in the pecan.xml, or names in 
    # config.xml.
    pftnums <- sapply(confxml, '[[', 'num')
    pftnames <- sapply(settings$pfts, '[[', 'name')
    names(pftnames) <- pftnums

    #### Common variables ####

    # PFT by cohort
    pft_co <- ncdf4::ncvar_get(nc, "PFT")

    # Patch area
    patch_area <- ncdf4::ncvar_get(nc, "AREA")

    # Create a patch index indicator vector
    patch_index <- patch_cohort_index(nc)

    forecast <- list()

    for (var_name in var.names) {

        pft_full_names <- paste("pft", pftnames,
                                sep = name_separator)
        names(pft_full_names) <- pftnums

        ## TODO: Convert to PEcAn standard names
        if (var_name == "AGB") {

            # Cohort AGB -- kgC plant-1
            agb_co_plant <- ncdf4::ncvar_get(nc, "AGB_CO")

            # Cohort stem density -- Plant m-2
            co_plant <- ncdf4::ncvar_get(nc, "NPLANT")

            # Cohort AGB -- kgC m-2
            agb_co <- agb_co_plant * co_plant

            # Aggregate AGB by patch and PFT
            agb_patch_pft <- tapply(agb_co, 
                                    list("PFT" = pft_co, "patch" = patch_index), 
                                    sum)

            # AGB by PFT and area
            agb_pft_x_area <- apply(agb_patch_pft, 1, "*", patch_area)
            agb_pft <- colSums(agb_pft_x_area, na.rm = TRUE)
            
            names(agb_pft) <- pft_full_names[names(agb_pft)]
            forecast[[var_name]] <- agb_pft
        } else {
            PEcAn.logger::logger.error("Variable ", var_name,
                                      " not currently supported",
                                      " by read.restart.ED2")
        }
    }

    return(unlist(forecast))
} # read_restart.ED2


