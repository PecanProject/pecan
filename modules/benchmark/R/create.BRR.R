##-------------------------------------------------------------------------------------------------#
##' For each benchmark id, calculate metrics and update benchmarks_ensemble_scores
##'  
##' @name create.BRR
##' @title Create benchmark reference run and ensemble
##' @param ensemble.id id of ensemble run that will be used for benchmarking
##' @param con database connection
##' @export 
##' 
##' @author Betsy Cowdery 

create.BRR <- function(ens_wf, con){
  
  cnd1 <- ens_wf$hostname == fqdn() 
  cnd2 <- ens_wf$hostname == 'test-pecan.bu.edu' & fqdn() == 'pecan2.bu.edu'
  cnd3 <- ens_wf$hostname == 'pecan2.bu.edu' & fqdn() == 'test-pecan.bu.edu'
  
  
  if(cnd1|cnd2|cnd3){  # If the ensemble run was done on localhost, turn into a BRR
    
    settingsXML <- file.path(ens_wf$folder,"pecan.xml")
    
    # Automatically creates a new pecan.xml I think. Need to fix this. 
    clean <- clean.settings(settingsXML, "findme.xml")
    # Remove database & host information
    clean$database <- NULL 
    clean$host <- NULL
    clean$info <- NULL
    clean$outdir <- NULL
    str(clean)
    
    settings_xml <- toString(listToXml(clean, "pecan"))
    
    ref_run <- db.query(paste0(" SELECT * from reference_runs where settings = '", settings_xml,"'"),con)
    
    if(length(ref_run) == 0){ # Make new reference run entry
      ref_run <- db.query(paste0("INSERT INTO reference_runs",
                                 "(model_id, settings, user_id, created_at, updated_at)",
                                 "VALUES(",ens_wf$model_id,", '",settings_xml,"' , ",user_id,
                                 ", NOW() , NOW()) RETURNING *;"),con)
    }else if(dim(ref_run)[1] > 1){# There shouldn't be more than one reference run with the same settings
      logger.error("There is more than one reference run in the database with these settings. Review for duplicates. ")
    }
    
    bm.ensemble <- db.query(paste0("INSERT INTO benchmarks_ensembles",
                                   "(reference_run_id, ensemble_id, model_id, ",
                                   "user_id, created_at, updated_at, citation_id)",
                                   "VALUES(",ref_run$id,",",ens_wf$ensemble_id,",", ref_run$model_id,", ",user_id,
                                   ", NOW() , NOW(), 1000000001 ) RETURNING *;"),con)

    bm.ensemble <- rename(bm.ensemble, bm_ensemble_id = id)
    BRR <- ref_run %>% rename(.,reference_run_id = id) %>% left_join(.,bm.ensemble) 
    
    return(BRR)
  }else{logger.error(sprintf("Cannot create a benchmark reference run for a run on hostname: %s", 
                             ens_wf$hostname))}
} #create.BRR
