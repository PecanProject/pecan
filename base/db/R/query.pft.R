##'  select plant id's associated with pft
##'
##' @title Query species given pft name
##' @name query.pft_species
##' @param modeltype type of model that is used, this is used to distinguish between different pfts with the same name.
##' @param pft string pft name
##' @param con database connection
##' @return data.frame containing id, genus, species, scientificname of each species associated with pft
##' @export query.pft_species
##' @author David LeBauer
##' @examples
##' \dontrun{
##' query.pft_species('ebifarm.pavi')
##' query.pft_species(settings = read.settings("pecan.xml"))
##' }
query.pft_species <- function(pft, modeltype = NULL, con) {
  # create pft subquery
  if (is.null(modeltype)) {
    query <- paste0("select species.id, species.genus, species.species, species.scientificname",
                    " from species, pfts, pfts_species",
                    " where species.id=pfts_species.specie_id",
                    " and pfts.id=pfts_species.pft_id",
                    " and pfts.pft_type='plant'",
                    " and pfts.name='", pft, "'")
  } else {
    query <- paste0("select species.id, species.genus, species.species, species.scientificname",
                    " from species, pfts, pfts_species, modeltypes",
                    " where species.id=pfts_species.specie_id",
                    " and pfts.id=pfts_species.pft_id",
                    " and pfts.pft_type='plant'",
                    " and pfts.name='", pft, "'",
                    " and pfts.modeltype_id=modeltypes.id",
                    " and modeltypes.name='", modeltype, "'")
  }

  species <- db.query(query = query, con = con)
  invisible(species)
}
#==================================================================================================#

##' Select cultivars associated with a PFT
##'
##' Given a PFT name and optionally a modeltype, finds its pft_id and
##' returns the cultivars associated with it.
##'
##' @details A PFT is allowed to have associated species or associated
##' cultivars, but not both. If this function returns no results, try checking
##' your PFT with \code{\link{query.pft_species}} instead.
##' Note that the cultivars associated with one PFT *are* allowed to come from
##' multiple species, if desired.
##'
##' @inheritParams query.pft_species
##' @return tibble containing names and ids for each cultivar
##'   and the species it comes from
##' @export
query.pft_cultivars <- function(pft, modeltype = NULL, con) {

  pft_tbl <- (dplyr::tbl(con, "pfts")
    %>% dplyr::filter(.data$name == !!pft, .data$pft_type == "cultivar"))

  if (!is.null(modeltype)) {
    pft_tbl <- (pft_tbl
      %>% dplyr::inner_join(
        dplyr::tbl(con, "modeltypes"),
        by = c("modeltype_id" = "id"),
        suffix = c("", ".mt"))
      %>% dplyr::filter(.data$name.mt == !!modeltype))
  }

  (pft_tbl
    %>% dplyr::inner_join(
      dplyr::tbl(con, "cultivars_pfts"),
      by = c("id" = "pft_id"),
      suffix = c("", ".cvpft"))
    %>% dplyr::inner_join(
      dplyr::tbl(con, "cultivars"),
      by = c("cultivar_id" = "id"),
      suffix = c("", ".cv"))
    %>% dplyr::inner_join(
      dplyr::tbl(con, "species"),
      by=c("specie_id" = "id"),
      suffix=c("", ".sp"))
    %>% dplyr::select(
      id = "cultivar_id",
      "specie_id",
      "genus",
      "species",
      "scientificname",
      cultivar = "name.cv")
    %>% dplyr::collect())
}

####################################################################################################
### EOF.  End of R script file.
####################################################################################################
