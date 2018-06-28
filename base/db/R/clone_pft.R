##' Duplicate existing pft with associated priors, species, and cultivars
##'
##' Creates a new pft that is a duplicate of an existing pft,
##' including relationships with priors, species, and cultivars (if any) of the existing pft.
##' This function mimics the 'clone pft' button in the PFTs record view page in the 
##' BETYdb web interface for PFTs that aggregate >=1 species, but adds the ability to 
##' clone the cultivar associations.
##'
##' @param parent.pft.name name of PFT to duplicate
##' @param new.pft.name name for new PFT. Must not be the same as parent.pft.name
##' @param new.pft.definition text for the new PFT's definition field.
##' @param settings PEcAn settings list, used only for BETYdb connection parameters
##' @return ID of the newly created pft in database, creates new PFT as a side effect
##' @author David LeBauer, Chris Black
##' @examples \dontrun{
##' clone_pft(parent.pft.name    = "tempdecid",
##'           new.pft.name       = "mytempdecid",
##'           new.pft.definition = "mytempdecid is a new pft",
##'           settings = pecan_settings_list)
##' }
clone_pft <- function(parent.pft.name,
                      new.pft.name,
                      new.pft.definition,
                      settings){

  if (new.pft.name == parent.pft.name) {
    PEcAn.logger::logger.severe("new.pft.name must not be the same as parent.pft.name")
  }

  if (length(new.pft.name) > 1 || length(parent.pft.name) > 1) {
    PEcAn.logger::logger.severe("multiple PFT names given, and clone_pft only knows how to handle one at a time.")
  }

  con <- db.open(settings$database$bety)
  on.exit(db.close(con))

  parent.pft <- (dplyr::tbl(con, "pfts")
    %>% dplyr::filter(name == parent.pft.name)
    %>% dplyr::collect())

  if (nrow(parent.pft) == 0) {
    PEcAn.logger::logger.severe("No existing PFT named '", parent.pft.name, "' found in database")
  }

  new.pft <- (parent.pft
    %>% dplyr::select(-id, -created_at, -updated_at)
    %>% dplyr::mutate(
      name = new.pft.name,
      definition = new.pft.definition,
      parent_id = parent.pft$id))

  ## create new pft
  DBI::dbWriteTable(
    conn = con,
    name = "pfts",
    value = as.data.frame(new.pft),
    append = TRUE,
    row.names = FALSE)

  new.pft$id <- (dplyr::tbl(con, "pfts")
    %>% dplyr::filter(name == new.pft.name)
    %>% dplyr::pull(id))


  # PFT members are stored in different tables depending on pft_type.
  # Both tables have two columns:
  #   - `pft_id`, which we update;
  #   - `cultivar_id` or `specie_id`, which we copy unchanged
  if (new.pft$pft_type == "cultivar") {
    member_tbl <- "cultivars_pfts"
  } else {
    member_tbl <- "pfts_species"
  }
  new_members <- (dplyr::tbl(con, member_tbl)
    %>% dplyr::filter(pft_id == parent.pft$id)
    %>% dplyr::mutate(pft_id = new.pft$id)
    %>% dplyr::distinct()
    %>% dplyr::collect())

  if(nrow(new_members) > 0){
    DBI::dbWriteTable(
      conn = con,
      name = member_tbl,
      value = as.data.frame(new_members),
      append = TRUE,
      row.names = FALSE)
  }

  new_priors <- (dplyr::tbl(con, "pfts_priors")
    %>% dplyr::filter(pft_id == parent.pft$id)
    %>% dplyr::mutate(pft_id = new.pft$id)
    %>% dplyr::distinct()
    %>% dplyr::collect())

  if(nrow(new_priors) > 0){
    DBI::dbWriteTable(
      conn = con,
      name = "pfts_priors",
      value = as.data.frame(new_priors),
      append = TRUE,
      row.names = FALSE)
  }

  return(new.pft$id)
}
