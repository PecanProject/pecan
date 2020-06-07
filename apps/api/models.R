#' Retrieve the details of a particular version of a model
#' @param name Model name (character)
#' @param revision Model version/revision (character)
#' @return Model details
#' @author Tezan Sahu
#* @get /
getModels <- function(model_name="all", revision="all", res){
  settings <-list(database = list(bety = list(
    driver = "PostgreSQL", 
    user = "bety", 
    dbname = "bety", 
    password = "bety", 
    host="postgres"
  )))
  dbcon <- PEcAn.DB::db.open(settings$database$bety)

  qry_statement <- "SELECT m.id AS model_id, m.model_name, m.revision, m.modeltype_id, t.name AS model_type FROM models m, modeltypes t WHERE m.modeltype_id = t.id"
  if (model_name == "all" & revision == "all"){
    # Leave as it is
  }
  else if (model_name != "all" & revision == "all"){
    qry_statement <- paste0(qry_statement, " and model_name = '", model_name, "'")
  }
  else if (model_name == "all" & revision != "all"){
    qry_statement <- paste0(qry_statement, " and revision = '", revision, "'")
  }
  else{
    qry_statement <- paste0(qry_statement, " and model_name = '", model_name, "' and revision = '", revision, "'")
  }

  qry_statement <- paste0(qry_statement, " ORDER BY m.id DESC")
  
  qry_res <- PEcAn.DB::db.query(qry_statement, dbcon)
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Model(s) not found"))
  }
  else {
    qry_res
  }
}
    
