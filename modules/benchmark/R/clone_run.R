##-----------------------------------------------------------------------------#
##' Produce a "cloned" run from the settings of a completed run
##' 
##' @name clone_run 
##' @title Clone Run
##' @param settings settings or multisettings object 
##' @param model model id 
##' @param con database connection
##' @export 
##' 
##' @author Betsy Cowdery 
##' @importFrom dplyr tbl filter rename collect select 

clone_run <- function(settings, model, con){
  if(is.MultiSettings(settings)) {
    return(invisible(papply(settings, clone_run, model, con))) 
  }
  
  # For testing: 
  # bety <- betyConnect("web/config.php")
  # con <- bety$con
  # settings <- read.settings("/fs/data2/output/PEcAn_1000008371/pecan.CHECKED.xml")
  # model = 1000000020
  
  settings.old <- settings 
  settings.new <- list()
  
  # Copy over settings that won't change
  settings.new$meta.analysis <- settings.old$meta.analysis
  settings.new$ensemble <- settings.old$ensemble
  settings.new$run$site <- settings.old$run$site
  settings.new$run$start.date <- settings.old$run$start.date
  settings.new$run$end.date <- settings.old$run$end.date
  
  # New model info
  model.new <- tbl(bety, "models") %>% filter(model == id) %>% collect()
  # List of all/required inputs for the new model 
  inputs_all <- tbl(bety, "modeltypes_formats") %>% filter(model.new$modeltype_id == modeltype_id) %>% select(one_of("tag", "required")) %>% collect
  inputs_req <- inputs_all$tag[inputs_all$required]
  
  #------ Update model ------#
  
  settings.new$model$id <- model.new$id
  
  #------ Update inputs ------#
  
  tags_std <- PEcAn.benchmark::clone_run_inputs
  
  
  for(tag in names(settings.old$run$inputs)){
    
    # Check that the given input can be used in the new model
    if(tag %in% inputs_all$tag){
      
      # Check lookup table to see what format input needs to be in 
      new_format <- c()
      
      # Check if old model input is in the correct format
      old.input <- tbl(bety, "inputs") %>% 
        filter(settings.old$run$inputs[[tag]]$id == id) %>% 
        select( one_of("id", "format_id", "parent_id")) %>% collect()
      
      if(!is.na(old.input$parent_id)){
        parent.input <- tbl(bety, "inputs") %>% 
          filter(old.input$parent_id == id) %>% 
          select( one_of("id", "format_id", "parent_id")) %>% collect()
      }else{
        parent.input <- list(format_id = NA)
      }
      
      if(old.input$format_id == new_format){
        # The input id points to a file that is already in the right format to be converted
        settings.new$run$inputs[[tag]]$id  <- old.input$id 
      }else if(parent.input$format_id == new_format){
        # The parent id points to a file that is in the right format to be converted
        settings.new$run$inputs[[tag]]$id  <- parent.input$id 
      }else if(is.na(old.input$parent_id)){
        # If the input has no parent id, assume it's raw data (is that safe???)
        settings.new$run$inputs[[tag]]$id  <- old.input$id
      }else{
        # This shouldn't ever happen
        # I assume model specific input shoud always have the standard as the parent
      }
      
    }else{
      # If input not needed, the tag could still be added to the new settings 
      # and be commented out inthe XML, but that would be difficult
      # to implement with a list object. I propose moivng it to an "omitted" list 
      # that could be written out in an XML or in some sort of report
    }
  }
  
  # Check that all required inputs are in settings
  missing_inputs <- setdiff(names(settings.new$run$inputs), inputs_req)
  if(length(missing_inputs)>0){
    PEcAn.utils::logger.warn("Required inputs were not specified in the old run settings")
    # If there are obvious defaults, use them to fill in missing inputs
    # Make sure the XML can't be run if there are missing inputs
  }
  
  #------ Update PFTs ------#

  old.pfts <- unlist(lapply(seq_along(pfts), function(x) settings$pfts[[x]][["name"]]))
  # Get all possible pfts for the model 
  new.pfts.all <- tbl(bety,"pfts") %>% filter(model.new$modeltype_id == modeltype_id) %>% pull(name)

  # Match the two sets of PFTs using align_pft
  # Note: "model_to_model" has not been implemented yet
  align_pft(con, old.pfts, new.pfts.all, comparison_type="model_to_model")
  
  # If PFTs have a unique match, insert in to settings.new
  # If PFTs have more than one match, insert multiple with warning that one must 
  #   be selected and the others deleted before the XML can be used
  # If PFTs have no match, warning that PFTs will need to be added manually
  
}