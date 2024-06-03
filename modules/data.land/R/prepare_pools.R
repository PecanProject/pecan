##' @name prepare_pools
##' @title prepare_pools
##' @description Calculates pools from given initial condition values, deriving complements where necessary/possible if given TotLivBiomass
##' @export
##'
##' @param nc.path path to netcdf file containing standard dimensions and variables; currently supports these variables: TotLivBiom, leaf_carbon_content, LAI, AbvGrndWood, root_carbon_content, fine_root_carbon_content, coarse_root_carbon_content, litter_carbon_content, soil_organic_carbon_content, soil_carbon_content, wood_debris_carbon_content
##' @param constants list of constants; must include SLA in m2 / kg C if providing LAI for leaf carbon
##' @return list of pool values in kg C / m2 with generic names
##' @author Anne Thomas
prepare_pools <- function(nc.path, constants = NULL){
  #function to check that var was loaded (numeric) and has a valid value (not NA or negative)
  is.valid <- function(var){
    return(all(is.numeric(var) && !is.na(var) &&  var >= 0)) 
  }
  
  IC.params <- list()
  
  if (!is.null(nc.path)) {
    IC.list <- PEcAn.data.land::pool_ic_netcdf2list(nc.path)
    if(!is.null(IC.list)){
      ### load biomass variables from IC list; will be NULL if not present (checked for later)
      TotLivBiom <- IC.list$vals$TotLivBiom
      leaf <- IC.list$vals$leaf_carbon_content
      LAI <- IC.list$vals$LAI
      wood <- IC.list$vals$wood_carbon_content
      AbvGrndWood <- IC.list$vals$AbvGrndWood
      roots <- IC.list$vals$root_carbon_content
      fine.roots <- IC.list$vals$fine_root_carbon_content
      coarse.roots <- IC.list$vals$coarse_root_carbon_content
      
      ### load non-living variables
      litter <- IC.list$vals$litter_carbon_content
      soil <- IC.list$vals$soil_organic_carbon_content
      wood.debris <- IC.list$vals$wood_debris_carbon_content
      
      # check if total roots are partitionable
      # note: if roots are partitionable, they will override fine_ and/or coarse_root_carbon_content if loaded
      if(is.valid(roots)){
        if("rtsize" %in% names(IC.list$dims)){
          PEcAn.logger::logger.info("prepare_pools: Attempting to partition root_carbon_content")
          rtsize <- IC.list$dims$rtsize
          part_roots <- PEcAn.data.land::partition_roots(roots, rtsize)
          if(!is.null(part_roots)){
            fine.roots <- part_roots$fine.roots
            coarse.roots <- part_roots$coarse.roots
          } else{
            PEcAn.logger::logger.error("prepare_pools: could not partition roots; please provide fine_root_carbon_content and coarse_root_carbon_content in netcdf.")
          }
        } else{
          PEcAn.logger::logger.error("prepare_pools: Please provide rtsize dimension with root_carbon_content to allow partitioning or provide fine_root_carbon_content and coarse_root_carbon_content in netcdf.")
        }
      } else{
        # proceed without error message
      }
      
      
      ### Calculate pools from IC list
      
      # initial canopy foliar carbon (kgC/m2)
      if (is.valid(leaf)) {
        IC.params[["leaf"]] <- leaf 
      } else if(is.valid(LAI)){
        sla <- constants$sla
        if(!is.null(sla)){
          leaf <- LAI * 1/sla
          PEcAn.logger::logger.info(paste("using LAI", LAI, "and SLA", sla, "to get leaf", leaf))
          IC.params[["leaf"]] <- leaf
        } else{
          PEcAn.logger::logger.error("Could not convert LAI to leaf carbon without SLA; please include 'constants' list with named element 'sla'")
        }
      } else if(is.valid(TotLivBiom) && is.valid(AbvGrndWood) && 
                is.valid(fine.roots) && is.valid(coarse.roots)){
        leaf <- (TotLivBiom - AbvGrndWood - fine.roots - coarse.roots) 
        if(leaf >= 0){
          IC.params[["leaf"]] <- leaf
        } else{
          PEcAn.logger::logger.error("TotLivBiom is less than sum of AbvGrndWood and roots; will use default for leaf biomass")
        }
      }
      #Calculate LAI given leaf and sla
      sla <- constants$sla
      if (!is.null(sla) && is.valid(leaf)) {
        LAI <- leaf * sla
        IC.params[["LAI"]] <- LAI
      }
      
      # initial pool of woody carbon (kgC/m2)
      if (is.valid(wood)){
        IC.params[["wood"]] <- wood
      } else if (is.valid(AbvGrndWood)) {
        if(is.valid(coarse.roots)){
          IC.params[["wood"]] <- (AbvGrndWood + coarse.roots) 
        } else{
          PEcAn.logger::logger.error("prepare_pools can't calculate total woody biomass with only AbvGrndWood; checking for total biomass.")
        }
      } else if (is.valid(TotLivBiom) && is.valid(leaf) && is.valid(fine.roots)){
        wood <- (TotLivBiom - leaf - fine.roots) 
        if (wood >= 0){
          IC.params[["wood"]] <- wood
        }else if ((leaf + fine.roots) < (TotLivBiom * 1.25)){
          PEcAn.logger::logger.error(paste("prepare_pools: Sum of leaf (", leaf, ") and fine roots(", fine.roots, ") is greater than TotLivBiom (", TotLivBiom, "); will reapportion with woody biomass."))
          #estimate new woody biomass and reapportion
          if(is.valid(coarse.roots)){
            #expand wood by coarse root to stem fraction (currently applied to both woody and non-woody)
            root.wood.frac <- 0.2 #deciduous forest, White et al.2000
            stem.wood.frac <- 0.8
            wood <- coarse.roots + ((stem.wood.frac * coarse.roots) / root.wood.frac) #cross multiply for stem wood and add 
            
          }else{
            wood <- 0
          }
          #reapportion wood, leaf and fine roots within TotLivBiom 
          leaf.new <- (leaf / (leaf + wood + fine.roots)) * TotLivBiom
          roots.new <- (fine.roots / (leaf + wood + fine.roots)) * TotLivBiom
          wood.new <- (wood / (leaf + wood + fine.roots)) * TotLivBiom
          IC.params[["wood"]] <- wood.new
          IC.params[["leaf"]] <- leaf.new
          IC.params[["fine.roots"]] <- roots.new
          PEcAn.logger::logger.info(paste("prepare_pools: Using", wood.new, "for wood, ", leaf.new, "for leaf,", roots.new, " for fine roots."))
        } else{
          PEcAn.logger::logger.severe(paste("prepare_pools: Sum of leaf (", leaf, ") and fine roots(", fine.roots, ") is more than 25% greater than TotLivBiom (", TotLivBiom, "); please check IC inputs."))
        }
      } else{
        PEcAn.logger::logger.error("prepare_pools could not calculate woody biomass; will use defaults. Please provide AbvGrndWood and coarse_root_carbon OR leaf_carbon_content/LAI, fine_root_carbon_content, and TotLivBiom in netcdf.")
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
          PEcAn.logger::logger.error("TotLivBiom is less than sum of AbvGrndWood, coarse roots, and leaf; will use default for fine.roots biomass")
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
      
      return(IC.params)
    }
    else{
      PEcAn.logger::logger.severe("Could not load initial conditions: output list is null")
      return(NULL)
    }
  }
  else{
    PEcAn.logger::logger.severe("Could not load initial conditions: filepath is null")
    return(NULL)
  }
}