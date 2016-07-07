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

create.BRR <- function(ensemble.id, workflow, con){
  
  cnd1 <- workflow$hostname == fqdn() 
  cnd2 <- workflow$hostname == 'test-pecan.bu.edu' & fqdn() == 'pecan2.bu.edu'
  cnd3 <- workflow$hostname == 'pecan2.bu.edu' & fqdn() == 'test-pecan.bu.edu'
  
  
  if(cnd1|cnd2|cnd3){  # If the ensemble run was done on localhost, turn into a BRR
    
    BRR <- db.query(paste0("INSERT INTO reference_runs (model_id, settings, user_id, created_at, updated_at) VALUES(",workflow$model_id,", '",workflow$folder,"' , ",user_id,", NOW() , NOW()) RETURNING *;"),con)
    
    bm.ensemble <- db.query(paste0("INSERT INTO benchmarks_ensembles (reference_run_id, ensemble_id, model_id, user_id, created_at, updated_at, citation_id) VALUES(",BRR$id,",",ensemble.id,",", BRR$model_id,", ",user_id,", NOW() , NOW(), 1000000001 ) RETURNING *;"),con)
    
    warning("Because you are creating a new reference run you will need to manually relate benchmarks.  This is done by 1) creating records for new data sources using the formats, inputs and dbfiles tables 2) Relating benchmarks with the reference run in the benchmarks_benchmarks_reference_runs table.")
    
    return(BRR)
  }else{logger.error(sprintf("Cannot create a benchmark reference run for a run on hostname: %s",  workflow$hostname))}
}
  
  
  # Case in which we would need a remote connection to get the pecan.xml file - not functional
  
  # else if(workflow$hostname != fqdn()){
  #   
  #   old.settings <- file.path(workflow$folder, "pecan.xml") 
  #   # some sort of remote call to copy over the contents of old.settings into new.settings 
  #   new.settings <- sprintf("pecan/%s.pecan.xml", basename(workflow$folder))
  #   
  #   cmdFcn <- sprintf("system('cat %s', intern=TRUE)",old.settings)
  #   result <- remote.execute.R(script=cmdFcn,workflow$hostname,user=,verbose=TRUE,R="R")
  #   writeLines(result, new.settings)
  #   clean.settings(new.settings,new.settings)
  #   
  #   system(sprintf("Rscript web/workflow.R %s", new.settings))
  #   return(settings)
  #   
  #   BRR <- db.query(paste0("INSERT INTO reference_runs (model_id, settings, user_id, created_at, updated_at) VALUES(",settings$model$id,", '",settings$outdir,"' , ",user_id,", NOW() , NOW()) RETURNING *;"),con)
  #   
  #   bm.ensemble <- db.query(paste0("INSERT INTO benchmarks_ensembles (reference_run_id, ensemble_id, model_id, user_id, created_at, updated_at, citation_id) VALUES(",BRR$id,",",ensemble$id,",", BRR$model_id,", ",user_id,", NOW() , NOW(), 1000000001 ) RETURNING *;"),con)
  #   }
