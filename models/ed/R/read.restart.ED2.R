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

    stop_year <- lubridate::year(stop.time)
    stop_month <- lubridate::month(stop.time)
    stop_day <- lubridate::day(stop.time)
    datetime_string <- sprintf("%04d-%02d-%02d-000000", 
                               stop_year, 
                               stop_month,
                               stop_day)
    histfile_string <- paste0("history-S-",
                              datetime_string,
                              ".*\\.h5")


    histfile <- list.files(histfile_path, 
                           histfile_string, 
                           full.names = TRUE)
    if (length(histfile) > 1) {
        PEcAn.utils::logger.error("Multiple history files found.")
    } else if (length(histfile) < 1) {
        PEcAn.utils::logger.error("No history files found.")
    } else {
        PEcAn.utils::logger.info("Using history file: ",
                                  histfile)
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
            PEcAn.utils::logger.error("Variable ", var_name,
                                      " not currently supported",
                                      " by read.restart.ED")
        }
    }

    return(unlist(forecast))
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

