##' This is a helper function for preparing PDA external objects, but it doesn't cover all the cases yet, use it with care
##' You can use this function just to generate either one of the external.* PDA objects, but note that some args cannot be blank depending on what you aim to generate
##' @param external.data boolean, if TRUE function will generate external.data for PDA, then you need to pass varn and obs too, as well as align_method if different than "match_timestep"
##' @param obs your data as a(n ordered) list where each sublist corresponds to a data frame of your constraining variable with two columns, variable name - posix
##' IMPORTANT: your obs must be in the same units as PEcAn standards already, this function doesn't do unit conversions!
##' IMPORTANT: your obs must be ready to compare with model outputs in general, e.g. if you're passing flux data it should already be ustar filtered
##' e.g.
##' obs\[\[1\]\]
##' NEE           posix 
##' 4.590273e-09  2017-01-01 00:00:00
##'           NA  2017-01-01 00:30:00
##'           NA  2017-01-01 01:00:00
##'           NA  2017-01-01 01:30:00
##'           NA  2017-01-01 02:00:00
##' 4.575248e-09  2017-01-01 02:30:00
##' if you have more than variable make sure the order you pass the data is the same as varn. E.g. for varn=c("NEE", "Qle"), external.data should be
##' obs\[\[1\]\]
##' NEE  posix
##' NA   2018-05-09
##' NA   2018-05-10
##' NA   2018-05-11
##' NA   2018-05-12
##' ... ... ...
##' obs\[\[2\]\]
##' Qle  posix
##' NA   2018-05-09
##' NA   2018-05-10
##' NA   2018-05-11
##' NA   2018-05-12
##' ... ... ...
##' @param varn a vector of PEcAn standard variable name(s) to read from model outputs, e.g. c("NEE", "Qle")
##' @param varid a vector of BETY variable id(s) of your constraints, e.g. for varn = c("NEE", "Qle"), varid = c(297, 298)
##' @param n_eff effective sample size of constraints, PDA functions estimates it for NEE and LE, and uses it in the heteroskedastic Laplacian only, if you already know it passing it now will save you some time
##' @param align_method one of the benchmark::align_data align_method options "match_timestep" or "mean_over_larger_timestep", defaults to "match_timestep"
##' @param par list with vector sublists of likelihood parameters of heteroskedastic laplacian for flux data, function calculates it if NULL for NEE, FC, and Qle. Leave empty for other variables
##' e.g. AMF.params <- PEcAn.uncertainty::flux.uncertainty(...fill in...)
##' par <- list(c(AMF.params$intercept, AMF.params$slopeP, AMF.params$slopeN))
##' @param model_data_diag optional for diagnostics, if you want to check whether your model and data will be aligned in PDA properly you can return a dataframe
##' as well as plot a quick & dirty timerseries graph
##' @param model.out an example model output folder to align your data with model, e.g. "/data/workflows/PEcAn_15000000111/out/15000186876"
##' @param start_date the start date of the model.out run, e.g. "2017-01-01"
##' @param end_date the end date of the model.out run, e.g. "2018-12-31"
##' @param external.formats boolean, if TRUE make sure to pass the varn argument
##' @param external.priors boolean, if TRUE pass prior.list argument too
##' @param prior.list a list of prior dataframes (one per pft, make sure the order is the same as it is in your <assim.batch> block), if you're using this make sure the targeted parameters are on the list
##' e.g.
##' prior.list <-  list(data.frame(distn = c("norm", "beta"), parama = c(4, 1),  paramb = c(7,2),   n = rep(NA, 2), row.names = c("growth_resp_factor", "leaf_turnover_rate")),
##'                     data.frame(distn = c("unif", "unif"), parama = c(10, 4), paramb = c(40,27), n = rep(NA, 2), row.names = c("psnTOpt", "half_saturation_PAR")))
##' @param external.knots boolean, if TRUE pass prior.list, ind.list, nknots OR knots.list arguments too
##' @param knots.list a list of dataframes (one per pft) where each row is a parameter vector, i.e. training points for the emulator. 
##' If not NULL these are used, otherwise knots will be generated using prior.list, ind.list and nknots.
##' @param ind.list a named list of vectors (one per pft), where each vector indicates the indices of the parameters on the prior.list targeted in the PDA 
##' e.g. ind.list <-  list(temperate.deciduous = c(2), temperate.conifer = c(1,2))
##' @param nknots number of knots you want to train the emulator on
##' @export
##' @examples
##' \dontrun{
##' pda.externals <-  pda.generate.externals(external.data   = TRUE, obs = obs, 
##' varn = "NEE", varid = 297, n_eff = 106.9386,
##' external.formats = TRUE, model_data_diag = TRUE, 
##' model.out = "/tmp/out/outdir",
##' start_date = "2017-01-01", end_date = "2018-12-31")
##' }

pda.generate.externals <-  function(external.data    = FALSE, obs = NULL, varn = NULL, varid = NULL, n_eff = NULL, align_method = "match_timestep", par = NULL,
                                    model_data_diag  = FALSE, model.out = NULL, start_date = NULL, end_date = NULL,
                                    external.formats = FALSE,
                                    external.priors  = FALSE, prior.list = NULL,
                                    external.knots   = FALSE, knots.list = NULL, ind.list = NULL, nknots = NULL){
  
  pda.externals <- list()
  ##################### external.data #####################
  if(external.data){
    if(is.null(obs) & is.null(varn) & is.null(varid)){
      stop("If you want to generate external.data, the following args cannot be NULL: obs, varn, varid")
    }
    external.data <-  vector("list", length(varn)) 
    for(i in seq_along(external.data)){
      
      # fill in external.data sublists : variable.name
      variable.name <-  list(variable.name = list(variable.drv = varn[i], variable.eqn = list(variables = varn[i], expression = varn[i])))
      external.data[[i]]$variable.name <-  variable.name
      
      # fill in external.data sublists : variable.id
      external.data[[i]]$variable.id <-  varid[i]
      
      # fill in external.data sublists : input.id
      external.data[[i]]$input.id <-  NA
      
      # fill in external.data sublists : align.method
      external.data[[i]]$align.method <-  align_method
      
      # fill in external.data sublists : data
      external.data[[i]]$data <-  obs[[i]]
      
      # fill in external.data sublists : obs
      external.data[[i]]$obs <-  obs[[i]][[varn[i]]]
      
      # fill in external.data sublists : par
      if(!is.null(par)){
        external.data[[i]]$par <- par[[i]]
      }else{
        AMF.params <- PEcAn.uncertainty::flux.uncertainty(measurement = external.data[[i]]$obs, 
                                                          QC = rep(0, length(external.data[[i]]$obs)), 
                                                          flags = TRUE, bin.num = 20)
        external.data[[i]]$par <- c(AMF.params$intercept, AMF.params$slopeP, AMF.params$slopeN)
      }
      
      # fill in external.data sublists : n
      external.data[[i]]$n <- sum(!is.na(external.data[[i]]$obs))
      
      # fill in external.data sublists : n_eff
      if(!is.null(n_eff)){
        external.data[[i]]$n_eff <- n_eff[i]
      }
    }
  }
  pda.externals$external.data <- external.data
  
  ##################### external.formats #####################
  if(external.formats){
    if(is.null(varn)){
      stop("If you want to generate external.formats, varn cannot be NULL.")
    }
    external.formats <- list()
    for(i in seq_along(varn)){
      external.formats[[i]] <-  list(vars = list(bety_name = varn[i], pecan_name = varn[i]))
    }
  }
  pda.externals$external.formats <- external.formats
  
  ##################### external.priors #####################
  if(external.priors){
    if(is.null(prior.list)){
      stop("If you want to generate external.priors, prior.list cannot be NULL.")
    }
    external.priors <- prior.list
  }
  pda.externals$external.priors <- external.priors
  
  ##################### external.knots #####################
  if(external.knots){
    #todo: generate external knots from prior.list
    if(is.null(knots.list) | (is.null(prior.list) & is.null(ind.list) & is.null(nknots))){
      stop("If you want to generate external.knots, please pass either prior.list, ind.list and nknots OR knots.list cannot args.")
    }else if(!is.null(knots.list)){
      external.knots <- knots.list
    }else if(!is.null(prior.list) & !is.null(ind.list) & !is.null(nknots)){
      prior.fcnx <- lapply(prior.list, pda.define.prior.fn)
      knots.list <- lapply(seq_along(prior.list),
                           function(x) pda.generate.knots(nknots, sf = NULL,
                                                          n.param.all = nrow(prior.list[[x]]),
                                                          prior.ind = ind.list[[x]],
                                                          prior.fn = prior.fcnx[[x]],
                                                          pname = row.names(prior.list[[x]])))
      names(knots.list) <- names(ind.list)
      external.knots <- lapply(knots.list, `[[`, "params")
    }
    
  }
  pda.externals$external.knots <- external.knots
  
  
  ##################### model & data alignment diagnostics #####################
  if(model_data_diag){
    if(is.null(obs) & is.null(model.out) & is.null(varn) & is.null(start_date) & is.null(end_date)){
      stop("If you want to check model data alignment diagnostics, the following args cannot be NULL: obs, model.out, varn, start_date, end_date")
    }
    model_data_diag <-  list()
    start_year <-  lubridate::year(start_date)
    end_year   <- lubridate::year(end_date)
    vars <- c("time", varn)
    model.raw <- as.data.frame(PEcAn.utils::read.output(basename(model.out), 
                                                        outdir = model.out,
                                                        start_year, end_year, variables = vars))
    model.secs <- PEcAn.utils::ud_convert(model.raw$time, "days" ,"seconds")
    model.raw$posix <- seq.POSIXt(from = as.POSIXlt(start_date, tz="GMT"), by = round(diff(model.secs)[1]), length.out = length(model.raw$time))
    
    for(i in seq_along(varn)){
      dat  <- PEcAn.benchmark::align_data(model.calc = model.raw, obvs.calc = obs[[i]], var = varn[i], align_method = align_method)
      obvz <- dat[,colnames(dat) == paste0(varn[i],".o"), drop = FALSE][[1]]
      modz <- dat[,colnames(dat) == paste0(varn[i],".m"), drop = FALSE][[1]]
      plot(obvz, main = "Model & data", ylab = varn[i], ylim = range(c(obvz, modz), na.rm = TRUE))
      graphics::points(modz, col = "red", pch = "+")
      graphics::legend("topleft", legend=c("model", "data"), pch=c("+", "o"), col=c("red", "black"))
      model_data_diag[[i]] <-  dat
    }
  }
  pda.externals$model_data_diag <- model_data_diag
  
  return(pda.externals)
}