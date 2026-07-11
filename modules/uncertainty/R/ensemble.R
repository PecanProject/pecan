#' Reads output from model ensemble
#'
#' Reads output for an ensemble of length specified by \code{ensemble.size} and bounded by \code{start.year}
#' and \code{end.year}
#'
#' @param ensemble.size the number of ensemble members run
#' @param pecandir specifies where pecan writes its configuration files
#' @param outdir directory with model output to use in ensemble analysis
#' @param start.year first year to include in ensemble analysis
#' @param end.year last year to include in ensemble analysis
#' @param variable target variables for ensemble analysis
#' @param ens.run.ids dataframe. Must contain a column named "id" giving the run IDs to be read.
#'   If NULL, will attempt to read IDs from a file named "samples.Rdata" in \code{pecandir}
#'
#' @return a list of ensemble model output
#' @export
#' @author Ryan Kelly, David LeBauer, Rob Kooper
#--------------------------------------------------------------------------------------------------#
read.ensemble.output <- function(ensemble.size, pecandir, outdir, start.year, end.year,
                                 variable, ens.run.ids = NULL) {

  # load manifest
  if (is.null(ens.run.ids)) {
    manifest <- utils::read.csv(file.path(pecandir, "runs_manifest.csv"), stringsAsFactors = FALSE)
    ens.run.ids <- manifest[manifest$type == "Ensemble", ]
  }

  expr <- variable$expression
  variables <- variable$variables

  ensemble.output <- list()

  # handles both (manifest has 'run_id', legacy 'ens.run.ids' might have 'id')
  run_ids <- ens.run.ids$run_id %||% ens.run.ids$id
  for (i in seq_along(run_ids)) {
    run.id <- run_ids[i]

    # We pass the whole 'variables' vector once, avoiding repeated file open/close.
    out.tmp <- PEcAn.utils::read.output(
      runid = run.id,
      outdir = file.path(outdir, run.id),
      start.year = start.year,
      end.year = end.year,
      variables = variables
    )

    # Assign loaded variables to local environment for evaluation
    for (var in names(out.tmp)) {
      assign(var, out.tmp[[var]])
    }

    # derivation & aggregation
    out <- eval(parse(text = expr))
    ensemble.output[[as.character(i)]] <- mean(out, na.rm = TRUE)
  }

  return(ensemble.output)
} # read.ensemble.output


#' Get parameter values used in ensemble
#'
#' Returns a matrix of randomly or quasi-randomly sampled trait values
#' to be assigned to traits over several model runs.
#' given the number of model runs and a list of sample distributions for traits
#' The model run is indexed first by model run, then by trait
#'
#' @param ensemble.size number of runs in model ensemble
#' @param pft.samples random samples from parameter distribution, e.g. from a MCMC chain
#' @param env.samples env samples
#' @param method the method used to generate the ensemble samples. Random generators: uniform, uniform with latin hypercube permutation. Quasi-random generators: halton, sobol, torus. Random generation draws random variates whereas quasi-random generation is deterministic but well equidistributed. Default is uniform. For small ensemble size with relatively large parameter number (e.g ensemble size < 5 and # of traits > 5) use methods other than halton.
#' @param param.names a list of parameter names that were fitted either by MA or PDA, important argument, if NULL parameters will be resampled independently
#' @param ... Other arguments passed on to the sampling method
#'
#' @return matrix of (quasi-)random samples from trait distributions
#' @export
#' @author David LeBauer, Istem Fer
get.ensemble.samples <- function( ensemble.size, pft.samples, env.samples,
                                 method = "random", param.names = NULL, ...) {

  # Define supported methods
  supported_methods <- c("random", "uniform", "halton", "sobol", "lhc")
  if (!method %in% supported_methods) {
    stop("Invalid sampling method")
  }

  ## force as numeric for compatibility with Fortran code in halton()
  ensemble.size <- as.numeric(ensemble.size)
  if (ensemble.size <= 0) {
    ans <- NULL
  } else if (ensemble.size == 1) {
    ans <- PEcAn.utils::get.sa.sample.list(pft.samples, env.samples, 0.5)
  } else {
    pft.samples[[length(pft.samples) + 1]] <- env.samples
    names(pft.samples)[length(pft.samples)] <- "env"
    pft2col <- NULL
    for (i in seq_along(pft.samples)) {
      pft2col <- c(pft2col, rep(i, length(pft.samples[[i]])))
    }

    total.sample.num <- sum(sapply(pft.samples, length))
    random.samples <- NULL


    if (method == "halton") {
      PEcAn.logger::logger.info("Using ", method, "method for sampling")
      random.samples <- randtoolbox::halton(n = ensemble.size, dim = total.sample.num, ...)
      ## force as a matrix in case length(samples) = 1
      random.samples <- as.matrix(random.samples)
    } else if (method == "sobol") {
      PEcAn.logger::logger.info("Using ", method, "method for sampling")
      random.samples <- randtoolbox::sobol(n = ensemble.size, dim = total.sample.num, scrambling = 3, ...)
      ## force as a matrix in case length(samples) = 1
      random.samples <- as.matrix(random.samples)
    } else if (method == "torus") {
      PEcAn.logger::logger.info("Using ", method, "method for sampling")
      random.samples <- randtoolbox::torus(n = ensemble.size, dim = total.sample.num, ...)
      ## force as a matrix in case length(samples) = 1
      random.samples <- as.matrix(random.samples)
    } else if (method == "lhc") {
      PEcAn.logger::logger.info("Using ", method, "method for sampling")
      random.samples <- PEcAn.emulator::lhc(t(matrix(0:1, ncol = total.sample.num, nrow = 2)), ensemble.size)
    } else if (method == "uniform") {
      PEcAn.logger::logger.info("Using ", method, "random sampling")
      # uniform random
      random.samples <- matrix(stats::runif(ensemble.size * total.sample.num),
                               ensemble.size,
                               total.sample.num)
    } else {
      PEcAn.logger::logger.info("Method ", method, " has not been implemented yet, using uniform random sampling")
      # uniform random
      random.samples <- matrix(stats::runif(ensemble.size * total.sample.num),
                               ensemble.size,
                               total.sample.num)
    }


    ensemble.samples <- list()
    sampled.indices <- list()

    col.i <- 0
    for (pft.i in seq(pft.samples)) {
      ensemble.samples[[pft.i]] <- matrix(nrow = ensemble.size, ncol = length(pft.samples[[pft.i]]))
      sampled.indices[[pft.i]] <- matrix(nrow = ensemble.size, ncol = length(pft.samples[[pft.i]]))

      # meaning we want to keep MCMC samples together
      if (length(pft.samples[[pft.i]]) > 0 && !is.null(param.names)) {
        if (method == "halton") {
          same.i <- floor(randtoolbox::halton(ensemble.size) * length(pft.samples[[pft.i]][[1]]))+1
        } else if (method == "sobol") {
          same.i <- floor(randtoolbox::sobol(ensemble.size, scrambling = 3) * length(pft.samples[[pft.i]][[1]]))+1
        } else if (method == "torus") {
          same.i <- floor(randtoolbox::torus(ensemble.size) * length(pft.samples[[pft.i]][[1]]))+1
        } else if (method == "lhc") {
          same.i <- floor(c(PEcAn.emulator::lhc(t(matrix(0:1, ncol = 1, nrow = 2)), ensemble.size) * length(pft.samples[[pft.i]][[1]])))+1
        } else if (method == "uniform") {
          same.i <- sample.int(length(pft.samples[[pft.i]][[1]]), ensemble.size)
        } else if (method == "random") {
            PEcAn.logger::logger.info("Using random row sampling for MCMC draws")
           same.i <- sample(nrow(pft.samples[[pft.i]][[1]]), ensemble.size, replace = TRUE)
        } else {
          PEcAn.logger::logger.error("Sampling method %s is not recognized", method)
        }
      }

      for (trait.i in seq(pft.samples[[pft.i]])) {
        col.i <- col.i + 1
        if (names(pft.samples[[pft.i]])[trait.i] %in% param.names[[pft.i]]) {
          ensemble.samples[[pft.i]][, trait.i] <- pft.samples[[pft.i]][[trait.i]][same.i]
          sampled.indices[[pft.i]][, trait.i] <- same.i
        } else {
          # Extract original trait values
          trait.values <- pft.samples[[pft.i]][[trait.i]]
          sampled.values <- stats::quantile(trait.values, random.samples[, col.i])

          ensemble.samples[[pft.i]][, trait.i] <- stats::quantile(pft.samples[[pft.i]][[trait.i]],
                                                                  random.samples[, col.i])
          sampled.indices[[pft.i]][, trait.i] <- sapply(sampled.values,
                                                        function(val) {which.min(abs(trait.values - val)) })
        }
      }
      ensemble.samples[[pft.i]] <- as.data.frame(ensemble.samples[[pft.i]])
      colnames(ensemble.samples[[pft.i]]) <- names(pft.samples[[pft.i]])

    }  #end pft
    names(ensemble.samples) <- names(pft.samples)
    ans <- ensemble.samples
  }

  return(list(ans,sampled.indices))
} # get.ensemble.samples


#' Write ensemble config files
#'
#' Writes config files for use in meta-analysis and returns a list of run ids.
#' Given a pft.xml object, a list of lists as supplied by get.sa.samples,
#' a name to distinguish the output files, and the directory to place the files.
#'
#' The restart functionality is developed using model specific functions by calling write_restart.modelname function.
#' First, you need to make sure that this function is already exist for your desired model.
#' See here \url{https://pecanproject.github.io/pecan-documentation/latest/pecan-models.html}.
#' new state is a dataframe with a different column for each state variable.
#' The number of the rows in this dataframe needs to be the same as the ensemble size.
#' The state variables that you can use for setting up initial conditions are model specific.
#' Check the documentation of the write_restart.<modelname> function for the model you are using.
#' The units for the state variables need to be in the PEcAn standard units which can be found in \link[PEcAn.utils]{standard_vars}.
#' new.params also has similar structure to ensemble.samples which is sent as an argument.
#'
#' @param input_design design matrix describing sampled inputs (see
#'   `run.write.configs()`). Columns named after `settings$run$inputs` tags give
#'   1-based indices into each input's `path` list and rows follow run order.
#'   Requires `nrow(input_design) >= ensemble.size`;
#'   extra rows are ignored.
#' @param ensemble.size size of ensemble
#' @param defaults pft
#' @param ensemble.samples list of lists supplied by \link{get.ensemble.samples}
#' @param settings list of PEcAn settings
#' @param model name of model to be run, e.g. "ED2" or "SIPNET"
#' @param clean remove old output first?
#' @param write.to.db logical: Record this run in BETY?
#' @param restart In case this is a continuation of an old simulation.
#'  Needs to be a list with name tags of runid, inputs, new.params (parameters),
#'  new.state (initial condition), ensemble.id (ensemble id), start.time and stop.time.
#'  See Details.
#' @param rename Decide if we want to rename previous output files, for example convert from sipnet.out to sipnet.2020-07-16.out.
#'
#' @return list, containing
#'  $runs = data frame of runids,
#'  $ensemble.id = the ensemble ID for these runs,
#'  and $samples with ids and samples used for each tag.
#'  Also writes sensitivity analysis configuration files as a side effect
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data %||%
#' @export
#' @author David LeBauer, Carl Davidson, Hamze Dokoohaki

write.ensemble.configs <- function(input_design , ensemble.size, defaults, ensemble.samples, settings, model,
                                   clean = FALSE, write.to.db = TRUE, restart = NULL, rename = FALSE) {

  # Check for required paths
  for (input_tag in names(settings$run$inputs)) {
    input <- settings$run$inputs[[input_tag]]
    input_paths <- input$path
    if (is.null(input_paths) || length(input_paths) == 0) {
      PEcAn.logger::logger.error("Input", sQuote(input_tag), "has no paths specified")
    }
    # Check for unsampled multi-path inputs
    if (length(input_paths) > 1 &&
          !(input_tag %in% names(settings$ensemble$samplingspace))) {
      PEcAn.logger::logger.error(
        "Input", sQuote(input_tag), "has", length(input_paths),
        "paths but no sampling method.",
        "Add <samplingspace> for this input in pecan.xml"
      )
    }
  }

  con <- NULL
  my.write.config <- paste("write.config.", model, sep = "")
  my.write_restart <- paste0("write_restart.", model)

  if (is.null(ensemble.samples)) {
    return(list(runs = NULL, ensemble.id = NULL))
  }

  # See if we need to write to DB
  if (!is.null(settings$database$bety$write)) {
    # specifying `write` in settings overrides write.to.db in fn args
    write.to.db <- as.logical(settings$database$bety$write)
  }

  if (write.to.db) {
    # Open connection to database so we can store all run/ensemble information
    con <-
      try(PEcAn.DB::db.open(settings$database$bety))
    on.exit(try(PEcAn.DB::db.close(con), silent = TRUE), add = TRUE)

    # If we fail to connect to DB then we set to NULL
    if (inherits(con, "try-error"))  {
      con <- NULL
      PEcAn.logger::logger.warn("We were not able to successfully establish a connection with BETYdb ")
    }
  }

  manifest_df <- data.frame(
    run_id = character(),
    site_id = character(),
    pft_name = character(),
    trait = character(),
    quantile = character(),
    type = character(),
    stringsAsFactors = FALSE
  )

  # Get the workflow id
  # if workflow$id is null, set to -1
  # to avoid collision w/ database ids
  workflow.id <- settings$workflow$id %||% -1

  #------------------------------------------------- if this is a new fresh run------------------
  if (is.null(restart)) {
    # create an ensemble id
    # Note: this ignores any existing settings$ensemble$id
    if (!is.null(con) && write.to.db) {
      # write ensemble first
      ensemble.id <- PEcAn.DB::db.query(paste0(
        "INSERT INTO ensembles (runtype, workflow_id) ",
        "VALUES ('ensemble', ", format(workflow.id, scientific = FALSE), ")",
        "RETURNING id"), con = con)[['id']]

      for (pft in defaults) {
        PEcAn.DB::db.query(paste0(
          "INSERT INTO posteriors_ensembles (posterior_id, ensemble_id) ",
          "values (", pft$posteriorid, ", ", ensemble.id, ")"), con = con)
      }
    } else {
      # Use existing id if provided, or an arbitrary unique value if not
      # Note: Since write.ensemble.configs is called separately for each site,
      # a multisite run with no ID provided gives each site its own ensemble id!
      ensemble.id <- settings$ensemble$id %||% rlang::hash(settings)
    }
    #-------------------------generating met/param/soil/veg/... for all ensembles----
    if (!is.null(con)){
      #-- lets first find out what tags are required for this model
      required_tags <- dplyr::tbl(con, 'models') %>%
        dplyr::filter(.data$id == !!as.numeric(settings$model$id)) %>%
        dplyr::inner_join(dplyr::tbl(con, "modeltypes_formats"), by = c('modeltype_id')) %>%
        dplyr::collect() %>%
        dplyr::filter(.data$required == TRUE) %>%
        dplyr::pull("tag")
    } else {
      required_tags <- c("met","parameters")
    }

    #now looking into the xml
    samp <- settings$ensemble$samplingspace
    #performing the sampling
    samples <- list()
    input_tags <- names(settings$run$inputs)
    for (input_tag in input_tags) {
      if (input_tag %in% colnames(input_design)) {
        input_paths <- settings$run$inputs[[input_tag]]$path
        input_indices <- input_design[[input_tag]]

        samples[[input_tag]] <- list(
          samples = lapply(input_indices, function(idx) input_paths[[idx]])
        )
      }
    }
    # if there is a tag required by the model but it is not specified in the xml then I replicate n times the first element
    required_tags %>%
      purrr::walk(function(r_tag) {
        if (is.null(samples[[r_tag]]) && r_tag != "parameters") {
          samples[[r_tag]]$samples <<- rep(settings$run$inputs[[tolower(r_tag)]]$path[1], ensemble.size)
        }
      })

    # Reading the site.pft specific tags from xml
    site.pfts.vec <- settings$run$site$site.pft %>%
      unlist() %>%
      as.character()

    if (!is.null(site.pfts.vec)) {
      # find the name of pfts defined in the body of pecan.xml
      defined.pfts <- settings$pfts %>%
        purrr::map("name") %>%
        unlist() %>%
        as.character()
      # subset ensemble samples based on the pfts that are specified in the site and they are also sampled from.
      if (length(which(site.pfts.vec %in% defined.pfts)) > 0) {
        ensemble.samples <-
          ensemble.samples [site.pfts.vec[which(site.pfts.vec %in% defined.pfts)]]
      }
      # warn if there is a pft specified in the site but it's not defined in the pecan xml.
      if (length(which(!(site.pfts.vec %in% defined.pfts))) > 0) {
        PEcAn.logger::logger.warn(
          paste0(
            "The following pfts are specified for the siteid ",
            settings$run$site$id ,
            " but they are not defined as a pft in pecan.xml:",
            site.pfts.vec[which(!(site.pfts.vec %in% defined.pfts))],
            collapse = ","
          )
        )
      }
    }

    # if no ensemble piece was in the xml I replicate n times the first element in params
    if ( is.null(samp$parameters) ) {
      samples$parameters$samples <- ensemble.samples %>% purrr::map(~.x[rep(1, ensemble.size) , ])
    }
    # This where we handle the parameters - ensemble.samples is already generated in run.write.config and it's sent to this function as arg -
    if ( is.null(samples$parameters$samples) ) {
      samples$parameters$samples <- ensemble.samples
    }
    #------------------------End of generating ensembles-----------------------------------
    # find all inputs that have an id
    inputs <- names(settings$run$inputs)
    inputs <- inputs[grepl(".id$", inputs)]

    # write configuration for each run of the ensemble
    runs <- data.frame()
    for (i in seq_len(ensemble.size)) {
      if (!is.null(con) && write.to.db) {
        paramlist <- paste("ensemble=", i, sep = "")
        # inserting this into the table and getting an id back
        run.id <- PEcAn.DB::db.query(paste0(
          "INSERT INTO runs (model_id, site_id, start_time, finish_time, outdir, ensemble_id, parameter_list) ",
          "values ('",
          settings$model$id, "', '",
          settings$run$site$id, "', '",
          settings$run$start.date, "', '",
          settings$run$end.date, "', '",
          settings$run$outdir, "', ",
          ensemble.id, ", '",
          paramlist, "') ",
          "RETURNING id"), con = con)[['id']]
        # associate inputs with runs
        if (!is.null(inputs)) {
          for (x in inputs) {
            PEcAn.DB::db.query(paste0("INSERT INTO inputs_runs (input_id, run_id) ",
                                      "values (", settings$run$inputs[[x]], ", ", run.id, ")"),
                               con = con)
          }
        }

      } else {

        run.id <- PEcAn.utils::get.run.id("ENS", PEcAn.utils::left.pad.zeros(i, 5), site.id = settings$run$site$id)

      }
      runs[i, "id"] <- run.id

      manifest_df <- rbind(manifest_df, data.frame(
        run_id = run.id,
        site_id = settings$run$site$id,
        pft_name = "NA",
        trait = "NA",
        quantile = "NA",
        type = "Ensemble",
        stringsAsFactors = FALSE
      ))

      # create folders (cleaning up old ones if needed)
      if (clean) {
        unlink(file.path(settings$rundir, run.id))
        unlink(file.path(settings$modeloutdir, run.id))
      }
      dir.create(file.path(settings$rundir, run.id), recursive = TRUE)
      dir.create(file.path(settings$modeloutdir, run.id), recursive = TRUE)

      # build dynamic input info string
      input_info <- ""
      #changing the structure of input tag to what the models are expecting
      for (input_i in seq_along(settings$run$inputs)) {
        input_tag <- names(settings$run$inputs)[[input_i]]
        if (!is.null(samples[[input_tag]])) {
          settings$run$inputs[[input_tag]][["path"]] <-
            samples[[input_tag]][["samples"]][[i]]
          input_info <- paste0(input_info,format(input_tag, width = 12, justify = "left"), ": ",
                               samples[[input_tag]]$samples[[i]], "\n")
        }
      }

      # write run information to disk
      cat("runtype     : ensemble\n",
          "workflow id : ", format(workflow.id, scientific = FALSE), "\n",
          "ensemble id : ", format(ensemble.id, scientific = FALSE), "\n",
          "run         : ", i, "/", ensemble.size, "\n",
          "run id      : ", format(run.id, scientific = FALSE), "\n",
          "pft names   : ", as.character(lapply(settings$pfts, function(x) x[["name"]])), "\n",
          "model       : ", model, "\n",
          "model id    : ", format(settings$model$id, scientific = FALSE), "\n",
          "site        : ", settings$run$site$name, "\n",
          "site  id    : ", format(settings$run$site$id, scientific = FALSE), "\n",
          input_info,  #  dynamically generated input paths
          "start date  : ", settings$run$start.date, "\n",
          "end date    : ", settings$run$end.date, "\n",
          "hostname    : ", settings$host$name, "\n",
          "rundir      : ", file.path(settings$host$rundir, run.id), "\n",
          "outdir      : ", file.path(settings$host$outdir, run.id), "\n",
          file = file.path(settings$rundir, run.id, "README.txt")
      )



      #changing the structure of input tag to what the models are expecting
      for (input_i in seq_along(settings$run$inputs)) {
        input_tag <- names(settings$run$inputs)[[input_i]]
        input <- settings$run$inputs[[input_tag]]

        if (!input_tag %in% names(samples)) {
          # Use first path (already validated as single path)
          settings$run$inputs[[input_tag]]$path <- unlist(input$path[1])
        } else {
          # Use sampled path
          settings$run$inputs[[input_tag]]$path <- samples[[input_tag]][["samples"]][[i]]
        }
      }



      do.call(my.write.config,
              args = list(
                defaults = defaults,
                trait.values = lapply(samples$parameters$samples, # this is the params
                                      function(x, n) { x[n, , drop = FALSE] },
                                      n = i),
                settings = settings,
                run.id = run.id
              )
      )
      cat(format(run.id, scientific = FALSE), file = file.path(settings$rundir, "runs.txt"), sep = "\n", append = TRUE)

    }
    return(invisible(list(runs = runs, ensemble.id = ensemble.id, samples = samples, manifest = manifest_df)))
    #------------------------------------------------- if we already have everything ------------------
  } else {
    #reading restart inputs
    inputs <- restart$inputs
    run.id <- restart$runid
    new.params <- restart$new.params
    new.state <- restart$new.state
    ensemble.id <- restart$ensemble.id

    # Reading the site.pft specific tags from xml
    site.pfts.vec <- settings$run$site$site.pft %>%
      unlist() %>%
      as.character()

    if (!is.null(site.pfts.vec)) {
      # find the name of pfts defined in the body of pecan.xml
      defined.pfts <- settings$pfts %>% purrr::map('name') %>% unlist %>% as.character
      # subset ensemble samples based on the pfts that are specified in the site and they are also sampled from.
      if (length(which(site.pfts.vec %in% defined.pfts)) > 0 ) {
        new.params <- new.params %>% purrr::map(~list(.x[[which(site.pfts.vec %in% defined.pfts)]],restart = .x$restart))
      }
      # warn if there is a pft specified in the site but it's not defined in the pecan xml.
      if (length(which(!(site.pfts.vec %in% defined.pfts)))>0) {
        PEcAn.logger::logger.warn(
          "The following pfts are specified for the siteid ",
          settings$run$site$id ,
          " but they are not defined as a pft in pecan.xml:",
          site.pfts.vec[which(!(site.pfts.vec %in% defined.pfts))]
        )
      }
    }

    #if ensemble folders do not exist create them
    for (j in 1:length(run.id)) {
      if (!file.exists(file.path(settings$rundir, run.id[[j]]))) {
        dir.create(file.path(settings$rundir, run.id[[j]]))
      }
    }

    # stop and start time are required by bc we are writing them down into job.sh
    for (i in seq_len(ensemble.size)) {
      input_list <- list()
      for (input_tag in names(inputs)) {
        # if it's the parameter list, skip.
        if (input_tag == "parameters") next
        if (!is.null(inputs[[input_tag]]$samples[[i]])) {
          input_list[[input_tag]] <- list(path = inputs[[input_tag]]$samples[[i]])
        }
      }

      do.call(my.write_restart,
              args =  list(outdir = settings$host$outdir,
                           runid = run.id[[i]],
                           start.time = restart$start.time,
                           stop.time = restart$stop.time,
                           settings = settings,
                           new.state = new.state[i, ],
                           new.params = new.params[[i]], #new.params$`646`[[i]] for debugging
                           inputs = input_list,
                           RENAME = rename)#for restart from previous model runs, not sharing the same outdir
      )

      manifest_df <- rbind(manifest_df, data.frame(
        run_id = run.id[[i]],
        site_id = settings$run$site$id,
        pft_name = "NA",
        trait = "NA",
        quantile = "NA",
        type = "Ensemble",
        stringsAsFactors = FALSE
      ))
    }
    params <- new.params
    return(invisible(
      list(runs = data.frame(id = run.id),
           ensemble.id = ensemble.id,
           samples = inputs,
           manifest = manifest_df)
    ))
  }

} # write.ensemble.configs



#' Generate an ensemble of samples from one input
#'
#' Given an input containing paths to multiple files, this function samples
#' from the given paths to make an ensemble of the requested size.
#' If method is `sampling` they are sampled with replacement from the full set;
#' if method is `looping` they are taken sequentially with the sequence
#' repeating as needed.
#'
#' If `parent_ids` is provided, it is used as the indices for the current input.
#' Use this when you have multiple inputs whose files should be sampled as a
#' coordinated set.
#' For example if your soil moisture files are matched to the rainfall amounts
#' in your weather files, generate the met ensemble first
#' (`met_ids <- input.ens.gen(..., input = "met", parent_ids = NULL)`)
#' and then pass those as parents to the soil moisture ensemble:
#' `input.ens.gen(..., input = "soilmoist", parent_ids = met_ids)`.
#'
#' If any indices in `parent_ids` are larger than the length of the input
#' being sampled (e.g. `53` when the input only has 25 paths),
#' the result is determined by `bad_parent_action`:
#' "error" stops execution and requests the user to fix their inputs,
#' "resample" uses the valid parents as-is and replaces the invalid parents
#' via method = "sampling".
#' Note that method "resample" is provided for backwards compatibility;
#' we strongly recommend validating parent-child alignment before calling
#' `input.ens.gen`, ideally in the same process that generates your parent ids.
#'
#' @param settings list of PEcAn settings
#' @param input name of input to sample, e.g. "met", "veg", "pss"
#' @param method Method for sampling - For now looping or sampling with replacement is implemented
#' @param parent_ids integer vector of indices to be used as-is
#' @param ensemble_size size of ensemble
#' @param bad_parent_action How to handle entries in `parent_id` that are not
#'  valid indices into the paths given for this input. See details
#'
#' @return A list with elements `id` showing the order of sampling
#'  and `samples` with the paths selected from those indices.
#' @export
#'
#' @examples
#' \dontrun{
#'   settings <- PEcAn.settings::read.settings("pecan.xml")
#'   input.ens.gen(
#'     settings,
#'     ensemble_size = 50,
#'     input = "met",
#'     method = "sampling"
#'   )
#' }
#'
input.ens.gen <- function(settings,
                          ensemble_size,
                          input,
                          method = "sampling",
                          parent_ids = NULL,
                          bad_parent_action = c("error", "resample")) {

  samples <- list()
  samples$ids <- c()

  if (is.null(method) && is.null(parent_ids)) return(NULL)

  # parameter is exceptional it needs to be handled spearatly
  if (input == "parameters") return(NULL)

  input_path <- settings$run$inputs[[tolower(input)]]$path
  if (is.null(input_path)) {
    PEcAn.logger::logger.error(
      "No paths found for input", sQuote(input), "in settings$run$inputs"
    )
  }

  if (!is.null(parent_ids)) {
    bad_parent_action <- match.arg(bad_parent_action)

    # Legacy support: versions through 1.9.0 expected parent_ids to be a list,
    # but only component `ids` was used
    if (is.list(parent_ids) && !is.null(parent_ids$ids)) {
      parent_ids <- parent_ids$ids
    }

    if (length(parent_ids) != ensemble_size || !is.numeric(parent_ids)) {
      PEcAn.logger::logger.error(
        "parent_ids must be an integer vector the same length as the ensemble")
    }

    out.of.sample <- !(parent_ids %in% seq_along(input_path))
    if (any(out.of.sample)) {
      if (bad_parent_action == "error") {
        PEcAn.logger::logger.error(
          "parent_ids must be valid indices for input paths. Bad values:",
          toString(parent_ids[out.of.sample])
        )
      } else if (bad_parent_action == "resample") {
        # sample for those that are outside the param size -
        # for example, parent id may send id number 200 but we have only 100 sample for param
        parent_ids[out.of.sample] <- sample(
          seq_along(input_path),
          sum(out.of.sample),
          replace = TRUE)
      }
    }
    samples$ids <- parent_ids
  } else if (tolower(method) == "sampling") {
    samples$ids <- sample(
      seq_along(input_path),
      ensemble_size,
      replace = TRUE)
  } else if (tolower(method) == "looping") {
    samples$ids <- rep_len(
      seq_along(input_path),
      length.out = ensemble_size)
  }
  #using the sample ids
  samples$samples <- input_path[samples$ids]

  return(samples)
}
