##-----------------------------------------------------------------------------#
##' Produce a "cloned" run from the settings of a completed run
##'  
##' @name clone_run 
##' @title Clone Run
##' @param settings settings or multisettings object 
##' @param model model name
##' @param con database connection
##' @export 
##' 
##' @author Betsy Cowdery 
##' @importFrom dplyr tbl filter rename collect select 

clone_run <- function(settings, model, con){
  
  settings.old <- settings.new <- settings 
  
  model.info <- PEcAn.DB::db.query(paste0("SELECT f.name, f.id, mt.type_string from modeltypes as m", 
                                          " join modeltypes_formats as mf on m.id = mf.modeltype_id", 
                                          " join formats as f on mf.format_id = f.id", 
                                          " join mimetypes as mt on f.mimetype_id = mt.id", 
                                          " where m.name = '", "DALEC", "' AND mf.tag='met'"), con)
  
  tags_req <- c() # List of required tags for the new model 
  tags_opt <- c() # List of optional tags for the new model 
  
  #------ Update model ------#
  settings.new$model <- NULL
  settings.new$model$id <- model.info$id
  
  #------ Update inputs ------#
  
  # Lookup table that lists the tags that are currently standardized and whether
  # or not the model in question takes a model specific version of the input
  # or if it takes the standardized version
  # 
  std_tags <- read.csv(
    system.file("Standardized_tags_lookup.csv", package = "PEcAn.benchmark"), stringsAsFactors = FALSE) %>% filter()
  
  
  
  for(i in seq_len(settings.old$run$inputs)){
    
    tag <- names(settings.old$run$inputs[i])
    
    # Check that the given tag is required/optional for the new model, 
    if(tag %in% tags_req | tag %in% tags_opt){
      
      # Look up the parent id of the input (which should be the pecan.standard)
      # Is there a standardized way of 
      
      
      if(tag %in% names(std_tags) & 
         std_tags[which(std_tags$model.name == model.info$model_name),tag] == "std"){
        # You only need to put in the parent ID
      }
    }else{
      # if not needed, remove from settings.new
      # Another option is to comment it out of the code, but that would be difficult
      # to implement with a list object. I propose moivng it to an omitted list 
      # that could be written out in an XML or in some sort of report
      settings.new$run$inputs[i] <- NULL
    }
    

    
  }
  
  
  
  
  
  
  # We don't need to fill in anything else right? That gets taken care of by 
  # other settings prep functions.  
  # 
  # model.info <- tbl(bety,"models") %>% filter(id == model) %>% collect()
  # settings.new$model$type <- model.info$model_name
  # settings.new$model$revision <- model.info$revision
  # settings.new$model$delete.raw <- FALSE #Should that be hardcoded?
  # settings.new$model$binary <- 
}