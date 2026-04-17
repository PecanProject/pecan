
# R/run_sobol_analysis.R



settings <- PEcAn.settings::read.settings("/projectnb/dietzelab/bthomas/pecan_runs/sipnet_test/pecan_updated.xml")
ensemble_size = settings$ensemble$size
if (PEcAn.settings::is.MultiSettings(settings)){
      sobol_obj <- PEcAn.uncertainty::generate_joint_ensemble_design(settings = settings[1], ensemble_size = ensemble_size, sobol = TRUE) 
}else{
      sobol_obj <- PEcAn.uncertainty::generate_joint_ensemble_design(settings = settings, ensemble_size = ensemble_size, sobol = TRUE) }
  
PEcAn.workflow::runModule.run.write.configs(settings,input_design = sobol_obj$X )
 
  
PEcAn.workflow::runModule_start_model_runs(settings, stop.on.error = stop_on_error)
  
 

sobol_results <- PEcAn.uncertainty::compute_sobol_indices(outdir = settings$outdir, 
                                   sobol_obj = sobol_obj, 
                                   var = "GPP") 
  

 


