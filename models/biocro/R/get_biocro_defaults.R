
# Retrieve Biocro::<dataname>
#
# Needs to be a function because `::`(pkg, name) treats its arguments as
# literals, so need to fully substitute before calling
from_bc <- function(dfname){
  do.call(`::`, list("BioCro", dfname))
}

#' Look for defaults provided as datasets in the BioCro model package
#'
#' When available, default parameters come in sets of three:
#' *_initial_values, *_parameters, *_modules
#'
#' @param genus Name of the genus (or really any string BioCro uses as a *_parameters prefix)
#' @return a list in the format expected by `BioCro::Gro()`,
#'   containing four lists named `type`, `initial_values`, `parameters`, and `modules`,
#'   or NULL if genus not found
#' @export
#'
get_biocro_defaults <- function(genus){

  default_names <- grep(
    pattern = genus,
    x = utils::data(package = "BioCro")$results[,"Item"],
    ignore.case = TRUE,
    value = TRUE)

  if (length(default_names) < 3) {
    PEcAn.logger::logger.error(
      "No default parameter sets for", genus, "found in BioCro")
    return(NULL)
  }

  genus_init_name <- grep("_initial_state$", default_names, value = TRUE)[[1]]
  genus_param_name <- grep("_parameters$", default_names, value = TRUE)[[1]]
  genus_module_name <- grep("_modules$", default_names, value = TRUE)[[1]]

  # Report the name BioCro uses
  # We're matching on prefixes only, so this may not be the same as `genus`
  biocro_genus_name <- gsub("_modules$", "", genus_module_name) 

  if (length(default_names) > 3) {
    PEcAn.logger::logger.error(
      "Multiple possible default parameter sets for", genus, "found in BioCro.",
      "Using '", biocro_genus_name, "', the first one found.",
      "If this is wrong, specify a parameter file in settings$pft$constants$file")
  }

  genus_photosynth <- sub(
    pattern = "^c([34]).*",
    replacement = "C\\1",
    x = from_bc(genus_module_name)$canopy_module_name)

  list(
    type = list(photosynthesis = genus_photosynth, genus = biocro_genus_name),
    initial_values = from_bc(genus_init_name),
    parameters = from_bc(genus_param_name),
    modules = from_bc(genus_module_name))
}
