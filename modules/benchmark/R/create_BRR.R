##' Create benchmark reference run and ensemble
##'
##' For each benchmark id, calculate metrics and update benchmarks_ensemble_scores
##'  
##' @param ens_wf table made from joining ensemble and workflow tables 
##' @param con database connection
##' @param user_id Optional user id to use for this record in reference_runs table
##' @export 
##' 
##' @author Betsy Cowdery 

create_BRR <- function(ens_wf, con, user_id = ""){
  
  # cnd1 <- ens_wf$hostname == PEcAn.remote::fqdn() 
  # cnd2 <- ens_wf$hostname == 'test-pecan.bu.edu' & PEcAn.remote::fqdn() == 'pecan2.bu.edu'
  # cnd3 <- ens_wf$hostname == 'pecan2.bu.edu' & PEcAn.remote::fqdn() == 'test-pecan.bu.edu'
  # if(cnd1|cnd2|cnd3){  # If the ensemble run was done on localhost, turn into a BRR
  
  clean <- PEcAn.benchmark::clean_settings_BRR(inputfile = file.path(ens_wf$folder,"pecan.CHECKED.xml"))
  settings_xml <- toString(PEcAn.settings::listToXml(clean, "pecan"))
  ref_run <- PEcAn.benchmark::check_BRR(settings_xml, con)
  
  if(length(ref_run) == 0){ # Make new reference run entry
    
    if(nchar(as.character(user_id)) > 0){
      cmd <- paste0("INSERT INTO reference_runs (model_id, settings, user_id)",
                    "VALUES(",ens_wf$model_id,", '",settings_xml,"' , ",user_id,
                    ") RETURNING *;")
    }else{
      cmd <- paste0("INSERT INTO reference_runs (model_id, settings)",
                    "VALUES(",ens_wf$model_id,", '",settings_xml,
                    "') RETURNING *;")
    }
    ref_run <- PEcAn.DB::db.query(cmd, con)
    
  }else if(dim(ref_run)[1] > 1){# There shouldn't be more than one reference run with the same settings
    PEcAn.logger::logger.error("There is more than one reference run in the database with these settings. Review for duplicates. ")
  }
  BRR <- ref_run %>% dplyr::rename(reference_run_id = .data$id)
  return(BRR)
  # }else{logger.error(sprintf("Cannot create a benchmark reference run for a run on hostname: %s", 
  #                            ens_wf$hostname))}
} #create_BRR
