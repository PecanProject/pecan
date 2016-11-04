# TODO: Change inheritParams to template (also need to create template)

#' @title State data assimilation read-restart for ED2
#'
#' @author Alexey Shiklomanov
#' @inheritParams PEcAn.SIPNET::read.restart.SIPNET
#' @examples
#' \dontrun{
#'   outdir <- "~/sda-hackathon/outputs"
#'   runid <- "99000000020"
#'   settings_file <- "outputs/pecan.CONFIGS.xml"
#'   settings <- PEcAn.settings::read.settings(settings_file)
#'   forecast <- read.restart.ED2
#' }
#' 
#' @export
read.restart.ED2 <- function(outdir, 
                             runid,
                             stop.time,
                             settings, 
                             var.names, 
                             params) {

    name_separator <- "."

    runid <- as.character(runid)
    rundir <- settings$host$rundir
    confxml_path <- file.path(rundir, runid, "config.xml")
    confxml <- XML::xmlToList(XML::xmlParse(confxml_path))

    histfile_path <- file.path(outdir, "out", runid)

    histfile <- list.files(histfile_path, "history-S-.*\\.h5", 
                           full.names = TRUE)
    # TODO: This needs to access the target date
    histfile <- tail(histfile, 1)
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

    # Common variables

    # PFT by cohort
    pft_co <- ncdf4::ncvar_get(nc, "PFT")

    # Patch length
    paco_N <- ncdf4::ncvar_get(nc, "PACO_N")

    # Patch area
    patch_area <- ncdf4::ncvar_get(nc, "AREA")

    # Create a patch index indicator vector
    patch_index <- do.call(c, mapply(rep, seq_along(paco_N), paco_N))

    forecast <- list()

    for (var_name in var.names) {

        pft_full_names <- paste(var_name, "pft", pftnames,
                                sep = name_separator)
        names(pft_full_names) <- pftnums

        ## TODO: Convert to PEcAn standard names
        if (var_name == "AGB") {

            # AGB by cohort -- long vector
            agb_co <- ncdf4::ncvar_get(nc, "AGB_CO")

            agb_patch_pft <- tapply(agb_co, 
                                    list("PFT" = pft_co, "patch" = patch_index), 
                                    sum)

            agb_pft_x_area <- apply(agb_patch_pft, 1, "*", patch_area)
            agb_pft <- colSums(agb_pft_x_area, na.rm = TRUE)
            names(agb_pft) <- pft_full_names[names(agb_pft)]
            forecast[[var_name]] <- agb_pft
        } else {
            PEcAn.utils::logger.error("Variable ", var_name,
                                      " not currently supported",
                                      " by read.restart.ED")
        }
    }

    return(forecast)
}



#### Notes...

# Calculate above-ground biomass
######
# AGB[coh] = stem_density(ico) * 

# Phony dims:
#   0 - All cohorts
#   1 - Sites 
#   2 - Size classes (11)
#   3 - PFTs (17)
#   4 - Patches
#   5 - Months?? 
#   6 - ??? 
#   7 - Something related to height
#   8 - Something to do with disturbance
#   9 - Soil layers (?)
#   10 - Something related to mortality
#   11 - Canopy radiation profile


# * Start with variables:
#     - AGB -- Calculated from:
#         - bdead(ico) -- BDEAD
#         - bleaf(ico) -- Calculated from:
#             - dbh(ico) -- DBH
#             - hite(ico) -- Calculated from dbh
#         - bsapwooda -- Calculated from:
#             - balive(ico) -- Calculated from bleaf
#             - hite(ico) -- Calculated from dbh

# Patch index
#paco_id <- ncdf4::ncvar_get(nc, "PACO_ID")

