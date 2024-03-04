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
##' @importFrom dplyr tbl filter rename collect select full_join

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
  
  #------ Update model ------#
  # New model info
  model.new <- tbl(bety, "models") %>% filter(model == id) %>% collect()
  settings.new$model$id <- model.new$id
  
  #------ Update inputs ------#
  
  # Table of all/required inputs for the new model 
  inputs_all <- tbl(bety, "modeltypes_formats") %>% 
    filter(model.new$modeltype_id == modeltype_id) %>% 
    select(one_of("tag", "required")) %>% collect
  
  # List of required inputs for the new model
  inputs_req <- inputs_all$tag[inputs_all$required]
  
  PEcAn.benchmark::clone_run_inputs
  # Inputs that can be converted from old to new via standardized formats
inputs_conv <- full_join(inputs_all, PEcAn.benchmark::clone_run_inputs, by = c("tag" = "new")) %>% 
    filter(old %in% names(settings.old$run$inputs))
  
  # Loop over the inputs that can be converted
  for(i in 1:nrow(inputs_conv)){
    
    tag_old <- inputs_conv[i,"old"]
    tag_new <- inputs_conv[i,"tag"]
    
    # Get the parent tree of the old input 
    family <- tbl(bety, "inputs") %>% 
      filter(settings.old$run$inputs[[tag_old]]$id == id) %>% 
      select( one_of("id", "format_id", "parent_id")) %>% collect()
    parent <- family$parent_id
    i = 1
    
    while(!is.na(parent)){ # There might be a better SQL query to find all parents, the first examples I found used while loops
      i = i + 1
      family[i,] <- tbl(bety, "inputs") %>% 
        filter(parent == id) %>% 
        select( one_of("id", "format_id", "parent_id")) %>% collect()
      parent <- family$parent_id[i]
    }
    
    # Which input entry to go back to 
    n <- which(family$format_id == inputs_conv$convert_format_id[i])
    settings.new$run$inputs[[tag_new]]$id  <- family$id[n]
  }
  
  # The remaining required inputs that weren't in the list of standardized formats
  # need to be recorded either in the xml or a separate 
  missing_inputs <- setdiff(names(settings.new$run$inputs), inputs_req)
  if(length(missing_inputs) > 0){
    PEcAn.utils::logger.warn("Required inputs were not specified in the old run settings")
    # If there are obvious defaults, use them to fill in missing inputs
    # Make sure the XML can't be run if there are missing inputs
  }
  
  #------ Update PFTs ------#
  
  old.pfts <- unlist(lapply(seq_along(pfts), function(x) settings$pfts[[x]][["id"]]))
  # Get all possible pfts for the model 
  new.pfts.all <- tbl(bety,"pfts") %>% filter(model.new$modeltype_id == modeltype_id) %>% pull(id)
  
  # Match the two sets of PFTs using align_pft
  # Note: "model_to_model" has not been implemented yet
  align_pft(con, old.pfts, new.pfts.all, comparison_type="model_to_model")
  
  # If PFTs have a unique match, insert in to settings.new
  # If PFTs have more than one match, insert multiple with warning that one must 
  #   be selected and the others deleted before the XML can be used
  # If PFTs have no match, warning that PFTs will need to be added manually
  
}