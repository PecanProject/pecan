#' A function to estimate individual alphas for Dirichlet distribution to
#' approximate the observed quantiles with means as known moments for SoilGrids
#' soil texture data.
#' Dirichlet distribution is assumed as soil texture data follow categorical
#' distribution and the probability of each category is in the range 0 to 1,
#' and all must sum to 1.
#'
#' @param means A vector of means of sand, clay, and silt proportion for one
#'  soil layer at one site from SoilGrids data
#' @param quantiles A list of 5th, 50th, and 95th percentiles for sand, clay,
#'  and silt for one soil layer at one site from SoilGrids data
#'
#' @examples
#' \dontrun{
#' # Means and percentiles for each category: sand, clay, and silt at one site and one depth
#' means <- c(0.566,0.193,0.241)
#' quantiles <-list(
#'   q5 = c(0.127,0.034,0.052), # 5th percentile
#'   q50 = c(0.615,0.15,0.191), # 50th percentile (median)
#'   q95 = c(0.799,0.66,0.616))  # 95th percentile
#' alpha_est <- estimate_dirichlet_parameters(means, quantiles)
#' }
#' @return The individual alphas that work best to fit the observed quantiles
#' @author Qianyu Li
estimate_dirichlet_parameters <- function(means, quantiles) {
  
  # A function to optimize alpha0, which is the sum of individual alphas.
  estimate_alpha0 <- function(means, quantiles) {
    # Objective function to minimize the difference between observed and simulated quantiles with means as a known moment
    objective_function <- function(alpha0) {
      if (alpha0 <= 0)
        return(Inf) # alpha0 couldn't be zero or negative as it is the sum of individual alpha which are positive reals
      # Estimate individual alpha based on that the means of each categorical data are individual alpha divided by alpha0 in Dirichlet distribution
      alpha <- means * alpha0
      # Generate samples based on estimated alpha
      samples <- MCMCpack::rdirichlet(10000, alpha) # Generate samples
      # Compute differences with observed quantiles
      estimated_quantiles <- apply(
        x = samples,
        margin = 2,
        FUN = stats::quantile,
        probs = c(0.05, 0.5, 0.95),
        na.rm = TRUE
      )
      quantile_diff <- sum((estimated_quantiles - do.call(rbind, quantiles))^2)
      return(quantile_diff)
    }
    
    # Optimize alpha0
    result <- stats::optim(
      par = 1, # Initial guess for alpha0
      fn = objective_function,
      method = "L-BFGS-B",
      lower = 0.01  # alpha0 must be positive
    )
    return(result$par)
  }
  
  alpha0 <- estimate_alpha0(means, quantiles)
  if (alpha0 <= 0) {
    stop("Estimated alpha0 is non-positive, which is invalid.")
  }
  alphas <- means * alpha0
  return(alphas)
}



#' A function to estimate the soil parameters based on SoilGrids soil texture
#'  data and write the parameter paths into settings
#'
#' @param settings A multi-site settings
#' @param sand,clay,silt Data frames containing fraction in percentage from SoilGrids250m
#'  v2.0, each with columns "Depth", "Quantile", "Siteid", and "Value"
#' @param outdir Provide the path to store the parameter files
#' @param write_into_settings Whether to write the path of parameter file into
#'  the setting. The default is TRUE
#'
#' @examples
#' \dontrun{
#'
#' outdir <- "/projectnb/dietzelab/Cherry/SoilGrids_texture/39NEON"
#' # each file contains percent salt, silt, or clay
#' sand <- readRDS("/path/to/SoilGrids_texture/sand_percent.rds")
#' clay <- readRDS("/path/to/SoilGrids_texture/clay_percent.rds")
#' silt <- readRDS("/path/to/SoilGrids_texture/silt_percent.rds")
#' settings <-read.settings("/path/to/pecan_monthly_SDA_soilwater.xml")
#' soil_params_ensemble_soilgrids(settings,sand,clay,silt,outdir)
#' }
#'
#' @return Ensemble soil parameter files defined in outdir and file paths in xml file
#' @export
#' @author Qianyu Li
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom foreach %dopar%

soil_params_ensemble_soilgrids <- function(settings,sand,clay,silt,outdir,write_into_settings=TRUE){
  
  # A function to rescale the sums of mean texture fractions to 1 as the original sums are slightly different from 1 for some layers
  rescale_sum_to_one <- function(sand, clay, silt) {
    total <- sand + clay + silt
    rescaled_sand <- sand / total
    rescaled_clay <- clay / total
    rescaled_silt <- silt / total
    return(list(
      sand = rescaled_sand,
      clay = rescaled_clay,
      silt = rescaled_silt))
  }
  
  # A function to write to settings
  create_mult_list <- function(list.names, paths) {
    out <- as.list(paths)
    names(out) <- list.names
    out
  }
  
  # Convert values to proportion (0-1) from percentage
  if (any(c(sand$Value, clay$Value, silt$Value) > 2)) {
    sand$Value <- if (is.null(sand$Value)) { NULL } else { sand$Value / 100 }
    clay$Value <- if (is.null(clay$Value)) { NULL } else { clay$Value / 100 }
    silt$Value <- if (is.null(silt$Value)) { NULL } else { silt$Value / 100 }
  }
  ens_n <- as.numeric(settings$ensemble$size)
  # Merge all soil texture data together
  texture_all <-merge(sand, clay, by=c("Depth", "Quantile", "Siteid"))  %>% merge(silt, by=c("Depth", "Quantile", "Siteid")) %>%
    `colnames<-`(c(
      "soil_depth", #"soil_depth" will be used in "soil2netcdf" function
      "quantile",
      "siteid",
      "fraction_of_sand_in_soil",
      "fraction_of_clay_in_soil",
      "fraction_of_silt_in_soil"))
  
  # Substitute the depth range with the bottom depth values in m (with the assumption that the first layer's top is at 0)
  texture_all$soil_depth <-
    gsub("100-200cm", 2, gsub("60-100cm", 1, gsub(
      "30-60cm", 0.6, gsub("15-30cm", 0.3, gsub(
        "5-15cm", 0.15, gsub("0-5cm", 0.05, texture_all$soil_depth))))))
  texture_all$soil_depth <- as.numeric(texture_all$soil_depth)
  # Reformat the list based on site id
  f1 <- factor(texture_all$siteid, levels = unique(texture_all$siteid))
  dat <- split(texture_all, f1)
  dat <- dat[order(as.numeric(names(dat)))] 
  # Grab Site IDs from settings
  settings_id <-lapply(settings, function(x) as.numeric(x$run$site$id)) %>% unlist()
  
  # initialize parallel.
  cl <- parallel::makeCluster(as.numeric(parallel::detectCores()))
  doSNOW::registerDoSNOW(cl)
  # setup progress bar.
  pb <- utils::txtProgressBar(min=1, max=length(dat), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  # loop over site.
  # foreach.
  PATH <- foreach::foreach(i = seq_along(dat), .packages = c("Kendall", "purrr", "PEcAn.data.land"), .options.snow=opts) %dopar% {
    samples_ens <- list()
    paths <- c()
    siteid <- as.numeric(unique(dat[[i]]$siteid))
    soil_depth <- unique(dat[[i]]$soil_depth)
    str_ns <- paste0(siteid %/% 1e+09, "-", siteid %% 1e+09)
    temp_outdir <- file.path(outdir, siteid)
    dir.create(temp_outdir)
    # Estimate Dirichlet parameters for each depth at each site
    for (depths in sort(unique(texture_all$soil_depth))) {
      quantiles <- list(
        q5 = dplyr::filter(dat[[i]], .data$quantile == "0.05", soil_depth == depths) %>%
          dplyr::select(
            "fraction_of_sand_in_soil",
            "fraction_of_clay_in_soil",
            "fraction_of_silt_in_soil"), # 5th percentile for each category
        q50 = dplyr::filter(dat[[i]], .data$quantile == "0.5", soil_depth == depths) %>%
          dplyr::select(
            "fraction_of_sand_in_soil",
            "fraction_of_clay_in_soil",
            "fraction_of_silt_in_soil"), # 50th percentile (median) for each category
        q95 = dplyr::filter(dat[[i]], .data$quantile == "0.95", soil_depth == depths) %>%
          dplyr::select(
            "fraction_of_sand_in_soil",
            "fraction_of_clay_in_soil",
            "fraction_of_silt_in_soil"))  # 95th percentile for each category
      
      # Extract the means
      means <- dplyr::filter(dat[[i]], .data$quantile == "Mean", soil_depth == depths) %>%
        dplyr::select(
          "fraction_of_sand_in_soil",
          "fraction_of_clay_in_soil",
          "fraction_of_silt_in_soil")
      soil_rescaled <-rescale_sum_to_one(means$fraction_of_sand_in_soil,means$fraction_of_clay_in_soil,means$fraction_of_silt_in_soil)
      
      # Replace the original means with the rescaled ones
      means$fraction_of_sand_in_soil <- soil_rescaled$sand
      means$fraction_of_clay_in_soil <- soil_rescaled$clay
      means$fraction_of_silt_in_soil <- soil_rescaled$silt
      
      # Estimate Dirichlet parameters
      alpha_est <- estimate_dirichlet_parameters(as.matrix(means), quantiles)
      
      # Generate the ensemble soil texture data based on the ensemble size (ens_n) defined in the settings
      samples <- MCMCpack::rdirichlet(ens_n, alpha_est)
      colnames(samples) <-c("fraction_of_sand_in_soil","fraction_of_clay_in_soil","fraction_of_silt_in_soil")
      samples <-list(samples) %>% stats::setNames(depths)
      samples_ens <- append(samples_ens, samples)
    }
    # Generate soil parameter file for each one in ensemble soil texture data
    for (ens in 1:ens_n) {
      # Choose one sample
      samples_all_depth <- lapply(samples_ens, function(x) x[ens, ])
      # Reformat the nested list as input to "soil2netcdf" function
      reformatted_soil_list <- reformat_soil_list(samples_all_depth)
      prefix <- paste0("Soil_params_", str_ns, "_", ens)
      new.file <-  file.path(outdir, siteid, paste0(prefix, ".nc"))
      out.ense <- soil2netcdf(reformatted_soil_list, new.file)
      paths <- c(new.file, paths)
    }
    return(paths)
  } %>% purrr::set_names(names(dat))
  # stop parallel.
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  
  # Write the parameter paths to settings
  if (write_into_settings) {
    for (i in seq_along(PATH)) {
      ind <- which(settings_id == names(PATH)[i])
      settings[[ind]]$run$inputs$soil_physics$source <- "SoilGrids"
      settings[[ind]]$run$inputs$soil_physics$output <- "soil_physics"
      settings[[ind]]$run$inputs$soil_physics$ensemble <- ens_n
      settings[[ind]]$run$inputs$soil_physics$path <-create_mult_list(rep("path", ens_n), PATH[[i]])
    }
    PEcAn.settings::write.settings(settings,outputdir = settings$outdir,outputfile = "pecan.xml")
  }
}

# A function to reformat the nested list as inputs to "soil2netcdf" function
reformat_soil_list <- function(samples_all_depth) {
  # Define the fractions we want to extract
  fractions <-
    c("fraction_of_sand_in_soil",
      "fraction_of_clay_in_soil",
      "fraction_of_silt_in_soil")
  
  # Initialize a new list to store reformatted data
  reformatted <- stats::setNames(vector("list", length(fractions)), fractions)
  
  # Extract data for each fraction
  for (fraction in fractions) {
    reformatted[[fraction]] <-
      unlist(lapply(samples_all_depth, function(depth_list) {
        depth_list[[fraction]] # Extract the fraction value
      })) %>% purrr::set_names(NULL)
  }
  # Combine depth into a single vector for readability
  reformatted$soil_depth <- as.numeric(names(samples_all_depth))
  return(reformatted)
}