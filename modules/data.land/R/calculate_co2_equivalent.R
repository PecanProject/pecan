.gwp_values <- list(
  AR4 = c(CH4 = 25, N2O = 298),
  AR5 = c(CH4 = 28, N2O = 265),
  AR6 = c(CH4 = 29.8, N2O = 273)
)

.co2_per_c <- 44 / 12

#' Convert SOC change and greenhouse gas fluxes to CO2 equivalents
#'
#' Converts soil organic carbon (SOC) stock change and CH4/N2O fluxes to
#' CO2-equivalent emissions using 100-year GWP values.
# TODO: consider extending to additional GWP time horizons supported by IPCC
# assessment reports (e.g., 20-year as well as 100-year values).
#'
#' @param delta_soc Numeric. Change in soil organic carbon as mass of elemental
#'   carbon (C).
#'   Positive values indicate soil carbon gain; negative values indicate loss.
#' @param ch4 Numeric. Methane emissions as mass of CH4.
#' @param n2o Numeric. Nitrous oxide emissions as mass of N2O.
#' @param gwp Character. IPCC report containing GWP100 values used: "AR6",
#'   "AR5", or "AR4".
#'
#' @return Numeric. Total CO2-equivalent emissions as mass of CO2e, expressed
#'   on the same spatial and temporal basis as the inputs. 
#'
#' @details
#' Inputs may use any mass units, but must be converted to consistent units
#' and expressed on the same spatial and temporal basis before summing.

#' Equations:
#'
#'   CO2e_SOC = -ΔSOC * (44 / 12)
#'
#'   CO2e_i = m_i * GWP100_i
#'
#' GWP values:
#'
#' - Default GWP100 values are from AR6 (IPCC, 2021).
#' - For CARB inventories, use "AR4" for comparability with current CARB inventories (CARB, 2025).
#'
#' @references
#' IPCC (2021). Climate Change 2021: The Physical Science Basis (WG1 AR6).
#' Cambridge University Press. https://doi.org/10.1017/9781009157896
#'
#' IPCC (2006). 2006 Guidelines for National Greenhouse Gas Inventories. IGES, Japan.
#'
#' IPCC (2019). 2019 Refinement to the 2006 IPCC Guidelines.
#' IPCC, Switzerland.
#'
#' Greenhouse Gas Protocol (2024). Global Warming Potential Values.
#' WRI/WBCSD. https://ghgprotocol.org/global-warming-potential-values
#'
#' California Air Resources Board (CARB), 2025. Greenhouse Gas Global Warming Potentials. https://ww2.arb.ca.gov/ghg-gwps
#'
#' @examples
#' to_co2e(delta_soc = 1)
#' to_co2e(ch4 = 1)
#' to_co2e(n2o = 1)
#' # return total over all sources
#' to_co2e(delta_soc = 1, ch4 = 0.1, n2o = 0.01)
#'
#' @export
to_co2e <- function(delta_soc = 0,
                    ch4 = 0,
                    n2o = 0,
                    gwp = c("AR6", "AR5", "AR4")) {
  gwp <- match.arg(gwp)

  co2_soc <- -delta_soc * .co2_per_c
  co2_ch4 <- ch4 * .gwp_values[[gwp]][["CH4"]]
  co2_n2o <- n2o * .gwp_values[[gwp]][["N2O"]]

  co2_sum <- co2_soc + co2_ch4 + co2_n2o
  return(co2_sum)
}
