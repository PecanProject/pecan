#' Extract ISCN SOC initial conditions from existing ISCN database.
#'
#' @param site_info Bety list of site info including site_id, lon, and lat.
#' @param ens ensemble number.
#' @param ecoregion.path path to the directory where you store the shape files of L1 and L2 ecoregion maps.
#'
#' @return A data frame containing sampled SOC, each row represent each site.
#' @export
#' 
#' @examples
#' @author Dongchen Zhang
#' @importFrom dplyr %>%
IC_ISCN_SOC <- function(site_info, ens = 100, ecoregion.path = NULL) {
  iscn_soc <- PEcAn.data.land::iscn_soc
  site_eco <- PEcAn.data.land::EPA_ecoregion_finder(site_info$lat, site_info$lon, ecoregion.path)
  soc <- iscn_soc[,which(colnames(iscn_soc) %in% site_eco$L2)]
  ic_sample_soc <- data.frame(matrix(NA, ens, length(site_info$site_id))) %>%
    `colnames<-`(site_info$site_id)
  for (i in seq_along(site_eco$L2)) {
    if (is.na(site_eco$L2[i])) {
      next
    }
    if (!site_eco$L2[i] %in% colnames(soc)) {
      next
    }
    ic_sample_soc[,i] <- PEcAn.utils::ud_convert(sample(soc[,site_eco$L2[i]], ens, replace = T), "g cm-2", "kg m-2")
  }
  return(ic_sample_soc)
}