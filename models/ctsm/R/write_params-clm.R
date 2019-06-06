#' Write CTSM Parameter File
#'
#' @param defaults
#' @param trait.values named list or data frame of traits, e.g.
#' \code{data.frame(vmax = 1, b0 = 2)} or \code{list(vmax = 1, b0 = 2)}
#' @param settings
#' @param run.id
#'
#' @return
#' @export
#'
#' @examples
#' /dontrun{
#' trait.values <- list(c4_grass = list(sla = 0.03, c2n_leaf = 35, stom_slope = 10, fineroot2leaf = 2, vcmax = 50))
#' write_params_ctsm(trait.values = trait.values, run.id = 1)
#' }

# TODO: update this to read param files (CTSM and FATES) out of the refcase directory, not the PEcAn package
# TODO: update to allow it to pick between CLM4.5 and CLM5 (CLM5 == CTSM) parameter set based on refcase, user selection
# TODO: match Pecan pft names with CTSM pft names when Pecan name is not identical to a CTSM name
## See issue https://github.com/PecanProject/pecan/issues/1008

write_params_ctsm <-
  function(defaults = system.file('clm5_params.c171117_0001.nc', package = 'PEcAn.CTSM'),
           trait.values,
           settings,
           run.id) {
    ## Copy and open default parameter files
    ctsm.param.default <-
      system.file('clm5_params.c171117_0001.nc', package = 'PEcAn.CTSM')
    if (!exists('local.rundir'))
      local.rundir <- tempdir()
    if (!exists('run.id'))
      run.id <- 1
    ctsm.param.file <-
      file.path(local.rundir, paste0("ctsm_params.", run.id, ".nc"))
    file.copy(ctsm.param.default, ctsm.param.file)
    ctsm.param.nc <- ncdf4::nc_open(ctsm.param.file, write = TRUE)
    
    ## Loop over PFTS
    npft <- length(trait.values)
    PEcAn.logger::logger.debug('there are ',
                               npft,
                               'PFTs in this run, they are named:',
                               names(trait.values))
    ctsm_pftnames <-
      stringr::str_trim(tolower(ncdf4::ncvar_get(ctsm.param.nc, "pftname")))
    for (i in seq_len(npft)) {
      pft.name <- names(trait.values)[i]
      if (is.null(pft.name) | is.na(pft.name)) {
        PEcAn.logger::logger.error("pft.name missing")
      } else {
        PEcAn.logger::logger.info(paste("PFT =", pft.name))
        PEcAn.logger::logger.debug(paste0("ctsm PFT number: ", which(ctsm_pftnames == pft.name)))
      }
      if (pft.name == 'env')
        next   ## HACK, need to remove env from default
      
      ## Match PFT name to COLUMN
      ipft <- match(tolower(pft.name), ctsm_pftnames)
      PEcAn.logger::logger.debug(paste0("CTSM pft index number: ", ipft))
      
      if (is.na(ipft)) {
        PEcAn.logger::logger.severe(
          paste(
            "Unmatched PFT",
            pft.name,
            "in CTSM PEcAn does not yet support non-default PFTs for this model"
          )
        )
      }
      
      ## Special variables used in conversions
      leafC <- 0.48
      fnr <- 7.16 #mass ratio of total Rubisco molecular mass to nitrogen in Rubisco (g Rubisco g-1 N in Rubisco)
      ar <- 60 #specific activity of Rubisco (µmol CO2 g-1 Rubisco s-1)

      ## Loop over VARIABLES
      pft.trait.values <- trait.values[[i]]
      for (v in seq_along(pft.trait.values)) {
        var <- names(pft.trait.values)[v]

        ### ----- Leaf physiological parameters

        if (var == "sla") { 
          ## default 0.03846
          ncdf4::ncvar_put(
            nc = ctsm.param.nc,
            varid = "slatop", 
            start = ipft,
            count = 1,
            vals = udunits2::ud.convert(pft.trait.values[1], "m2 kg-1", "m2 g-1") / leafC  #TODO: add conversion back in
          )
        }
        if (var == "c2n_leaf"){
          ## default 35.36068
          ncdf4::ncvar_put(
            nc = ctsm.param.nc, 
            varid = "leafcn", 
            start = ipft, 
            count = 1, 
            vals = pft.trait.values[v]
          )
        }
        if (var == "stom_slope"){
          ## default 9.757532
          ncdf4::ncvar_put(
            nc = ctsm.param.nc, 
            varid = "mbbopt", 
            start = ipft, 
            count = 1, 
            vals = pft.trait.values[v]
          )
        }
        if (var == "fineroot2leaf"){
          ## default 1.5
          ncdf4::ncvar_put(
            nc = ctsm.param.nc, 
            varid = "froot_leaf", 
            start = ipft, 
            count = 1, 
            vals = pft.trait.values[v]
          )
        }
        if (var == "vcmax"){
          ## default 0.09
          ncdf4::ncvar_put(
            nc = ctsm.param.nc,
            varid = "flnr",
            start = ipft,
            count = 1,
            vals = as.numeric(pft.trait.values[v]) / ((1/(ncdf4::ncvar_get(ctsm.param.nc, varid = "leafcn")[ipft] * ncdf4::ncvar_get(ctsm.param.nc, varid = "slatop")[ipft])) * fnr * ar)
          )
        }
      } ## end loop over VARIABLES
    } ## end loop over PFTs
  }


write_params_fates <-
  function(defaults = system.file('????', package = 'PEcAn.FATES'),
           trait.values,
           settings,
           run.id) {
    ncdf4::nc_close(fates.param.nc)
  }