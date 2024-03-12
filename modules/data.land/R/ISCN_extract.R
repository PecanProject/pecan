#' Extract ISCN SOC initial conditions from existing ISCN database.
#'
#' @param site_info Bety list of site info including site_id, lon, and lat.
#' @param ens ensemble number.
#'
#' @return A data frame containing sampled SOC, each row represent each site.
#' @export
#' 
#' @examples
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
IC_ISCN_SOC <- function(site_info, ens = 100) {
  iscn_soc <- PEcAn.data.land::iscn_soc
  site_eco <- PEcAn.data.land::EPA_ecoregion_finder(site_info$lat, site_info$lon)
  soc <- iscn_soc %>%
    dplyr::filter(
      .data$NA_L2CODE %in% site_eco$L2
    ) %>%
    dplyr::select("soc (g cm-2)", "NA_L2CODE") %>%
    na.omit()
  ic_sample_soc <- data.frame(matrix(NA, ens, length(site_info$site_id))) %>%
    `colnames<-`(site_info$site_id)
  for (i in seq_along(site_eco$L2)) {
    if (is.na(site_eco$L2[i])) {
      next
    } else {
      ic_sample_soc[,i] <- sample(soc$`soc (g cm-2)`[which(soc$NA_L2CODE == site_eco$L2[i])], ens, replace = T)
    }
  }
  return(ic_sample_soc)
}