#' Add a PFT or list of PFTs to a settings object
#'
#' @param settings Input PEcAn settings list
#' @param name PFT name (character)
#' @param pfts Either a character vector of PFT names, or a list of
#'   PFT objects (which must each include an element named "name")
#' @param ... Additional arguments for modifying the PFT.
#' @return Updated settings list with PFTs added
#' @author Alexey Shiklomanov
#' @examples
#' settings <- list()
#' add_pft(settings, "Optics.Temperate_Early_Hardwood")
#' add_pft_list(settings, sprintf("Temperate_%s_Hardwood", c("Early", "Mid", "Late")))
#' add_pft_list(
#'   settings,
#'   list(list(name = "deciduous", num = 3),
#'        list(name = "coniferous", num = 6))
#' )  
#' if (require("magrittr")) {
#'   list() %>%
#'     add_pft("early_hardwood") %>%
#'     add_pft("mid_hardwood") %>%
#'     add_pft("late_hardwood")
#' }
#' @export
add_pft <- function(settings, name, ...) {
  pft_list <- settings[["pfts"]]
  new_pft <- list(name = name, ...)
  settings[["pfts"]] <- c(pft_list, list(pft = new_pft))
  settings
}

#' @rdname add_pft
#' @export
add_pft_list <- function(settings, pfts, ...) {
  for (pft in pfts) {
    if (is.character(pfts)) {
      settings <- add_pft(settings, pft, ...)
    } else {
      args <- c(list(settings = settings), pft)
      settings <- do.call(add_pft, args)
    }
  }
  settings
}
