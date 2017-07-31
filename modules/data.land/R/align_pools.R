align_pools <- function(nc.path,sla=NULL){
  #sla must be converted to m2/kgC
  #function to check that ncvar was loaded (numeric) and has a valid value (not NA or negative)
  is.valid <- function(var){
    return(all(is.numeric(var) && !is.na(var) &&  var >= 0)) 
  }
  
  default.param <- read.table(system.file("default_param.dalec", package = "PEcAn.DALEC"), header = TRUE)
  IC.params <- list()
  
  if (!is.null(settings$run$inputs$poolinitcond$path)) {
    IC.list <- PEcAn.data.land::pool_ic_netcdf2list(nc.path)
    if(!is.null(IC.list)){
      ### load biomass variables from IC list; will be NULL if not present (checked for later)
      TotLivBiom <- IC.list$vals$TotLivBiom
      leaf <- IC.list$vals$leaf_carbon_content
      LAI <- IC.list$vals$LAI
      AbvGrndWood <- IC.list$vals$AbvGrndWood
      roots <- IC.list$vals$root_carbon_content
      fine.roots <- IC.list$vals$fine_root_carbon_content
      coarse.roots <- IC.list$vals$coarse_root_carbon_content
      
      ### load non-living variables
      litter <- IC.list$vals$litter_carbon_content
      soil <- IC.list$vals$soil_organic_carbon_content
      wood.debris <- IC.list$vals$wood_debris_carbon_content
      
      if(!all(sapply(c(TotLivBiom,leaf,LAI,AbvGrndWood,roots,fine.roots,coarse.roots),is.numeric))){
        PEcAn.utils::logger.info("DALEC IC: Any missing vars will be calculated from those provided or replaced by DALEC's defaults")
      }
      
      # check if total roots are partitionable
      # note: if roots are partitionable, they will override fine_ and/or coarse_root_carbon_content if loaded
      if(is.valid(roots)){
        if("rtsize" %in% names(IC.list$dims)){
          PEcAn.utils::logger.info("align_pools: Attempting to partition root_carbon_content")
          rtsize <- IC.list$dims$rtsize
          part_roots <- PEcAn.data.land::partition_roots(roots, rtsize)
          if(!is.null(part_roots)){
            fine.roots <- part_roots$fine.roots
            coarse.roots <- part_roots$coarse.roots
          } else{
            PEcAn.utils::logger.error("align_pools: could not partition roots; please provide fine_root_carbon_content and coarse_root_carbon_content in netcdf.")
          }
        } else{
          PEcAn.utils::logger.error("align_pools: Please provide rtsize dimension with root_carbon_content to allow partitioning or provide fine_root_carbon_content and coarse_root_carbon_content in netcdf.")
        }
      } else{
        # proceed without error message
      }
      
      
      ### Calculate pools from IC list
      
      # initial canopy foliar carbon (kgC/m2)
      if (is.valid(leaf)) {
        IC.params[["leaf"]] <- leaf 
      } else if(is.valid(LAI) && !is.null(sla)){
          leaf <- LAI * 1/SLA
          IC.params[["leaf"]] <- leaf
      } else if(is.valid(TotLivBiom) && is.valid(AbvGrndWood) && 
                is.valid(fine.roots) && is.valid(coarse.roots)){
        leaf <- (TotLivBiom - AbvGrndWood - fine.roots - coarse.roots) 
        if(leaf >= 0){
          IC.params[["leaf"]] <- leaf
        } else{
          PEcAn.utils::logger.error("TotLivBiom is less than sum of AbvGrndWood and roots; will use default for leaf biomass")
        }
      }
      
      # initial pool of woody carbon (kgC/m2)
      if (is.valid(AbvGrndWood)) {
        if(is.valid(coarse.roots)){
          IC.params[["wood"]] <- (AbvGrndWood + coarse.roots) 
        } else{
          PEcAn.utils::logger.error("align_pools can't calculate total woody biomass with only AbvGrndWood; checking for total biomass.")
        }
      } else if (is.valid(TotLivBiom) && is.valid(leaf) && is.valid(fine.roots)){
        wood <- (TotLivBiom - leaf - fine.roots) 
        if (wood >= 0){
          IC.params[["wood"]] <- wood
        }else{
          PEcAn.utils::logger.error(paste("TotLivBiom (", TotLivBiom, ") is less than sum of leaf (", leaf, ") and fine roots(",fine.roots,"); will use default for woody biomass."))
        }
      } else{
        PEcAn.utils::logger.error("align_pools could not calculate woody biomass; will use defaults. Please provide AbvGrndWood and coarse_root_carbon OR leaf_carbon_content/LAI, fine_root_carbon_content, and TotLivBiom in netcdf.")
      }
      
      # initial pool of fine root carbon (kgC/m2)
      if (is.valid(fine.roots)) {
        IC.params[["fine.roots"]] <- fine.roots 
      } else if(is.valid(TotLivBiom) && is.valid(AbvGrndWood) && 
                is.valid(leaf) && is.valid(coarse.roots)){
        fine.roots <- (TotLivBiom - AbvGrndWood - leaf - coarse.roots) 
        if(fine.roots >= 0){
          IC.params[["fine.roots"]] <- fine.roots
        } else{
          PEcAn.utils::logger.error("TotLivBiom is less than sum of AbvGrndWood, coarse roots, and leaf; will use default for fine.roots biomass")
        }
      }
      
      
      # initial pool of litter carbon (kgC/m2)
      if (is.valid(litter)) {
        IC.params[["litter"]] <- litter 
      }
      
      # initial pool of soil organic matter (kgC/m2)
      if(is.valid(soil)){
        IC.params[["soil"]] <- soil
      } else {
        soil <- IC.list$vals$soil_carbon_content
        if(is.valid(soil)){
          IC.params[["soil"]] <- soil 
        } 
      }
      
      # initial pool of woody debris (kgC/m2)
      if(is.valid(wood.debris)){
        IC.params[["wood.debris"]] <-sum(wood.debris)
      }
    }
  }
}