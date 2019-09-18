#' Create a PEcAn XML file and use it to run a PEcAn workflow
#'
#' @param model_id (numeric) Model ID (from `models` table)
#' @param met (character) Name of meteorology input source (e.g. `"CRUNCEP"`)
#' @param site_id (numeric) Site ID (from `sites` table)
#' @param start_date (character or date) Run start date
#' @param end_date (character or date) Run end date
#' @param dbfiles_folder (character) Path to `dbfiles` directory
#' @param user_id (numeric) User ID to associate with the workflow
#' @param output_folder (character) Path to root directory for storing outputs.
#'   Default = `"batch_test_output"`
#' @param pecan_path (character) Path to PEcAn source code. Default is current
#'   working directory.
#' @param pft (character) Name of PFT to run. If `NULL` (default), use the first
#'   PFT in BETY associated with the model.
#' @param ensemble_size (numeric) Number of ensembles to run. Default = 1.
#' @param sensitivity_variable (character) Variable for performing sensitivity
#'   analysis. Default = `"NPP"`
#' @param sensitivity (logical) Whether or not to perform a sensitivity analysis
#'   (default = `FALSE`)
#' @param db_bety_username,db_bety_password,db_bety_hostname,db_bety_port
#'   (character) BETY database connection options. Default values for all of
#'   these are pulled from `<pecan_path>/web/config.php`.
#' @param db_bety_driver (character) BETY database connection driver (default = `"Postgres"`)
#' @return A list with two entries:
#'  * `sys`: Exit value returned by the workflow (0 for sucess).
#'  * `outdir`: Path where the workflow results are saved
#' @author Alexey Shiklomanov, Tony Gardella
#' @importFrom dplyr %>% .data
#' @export
create_execute_test_xml <- function(model_id,
                                    met,
                                    site_id,
                                    start_date,
                                    end_date,
                                    dbfiles_folder,
                                    user_id,
                                    output_folder = "batch_test_output",
                                    pecan_path = getwd(),
                                    pft = NULL,
                                    ensemble_size = 1,
                                    sensitivity_variable = "NPP",
                                    sensitivity = FALSE,
                                    db_bety_username = NULL,
                                    db_bety_password = NULL,
                                    db_bety_hostname = NULL,
                                    db_bety_port = NULL,
                                    db_bety_driver = "Postgres") {

  php_file <- file.path(pecan_path, "web", "config.php")
  config.list <- PEcAn.utils::read_web_config(php_file)
  if (is.null(db_bety_username)) db_bety_username <- config.list$db_bety_username
  if (is.null(db_bety_password)) db_bety_password <- config.list$db_bety_password
  if (is.null(db_bety_hostname)) db_bety_hostname <- config.list$db_bety_hostname
  if (is.null(db_bety_port)) db_bety_port <- config.list$db_bety_port
  con <- PEcAn.DB::db.open(list(
    user = db_bety_username,
    password = db_bety_password,
    host = db_bety_hostname,
    port = db_bety_port,
    driver = db_bety_driver
  ))
  on.exit(PEcAn.DB::db.close(con), add = TRUE)

  settings <- list(
    info = list(notes = "Test_Run",
                userid = user_id,
                username = "None",
                dates = Sys.Date())
  )

  #Outdir
  model.new <- dplyr::tbl(con, "models") %>%
    dplyr::filter(.data$id == !!model_id) %>%
    dplyr::collect()
  outdir_pre <- paste(
    model.new[["model_name"]],
    format(as.Date(start_date), "%Y-%m"),
    format(as.Date(end_date), "%Y-%m"),
    met, site_id, "test_runs",
    sep = "_"
  )
  outdir <- file.path(output_folder, outdir_pre)
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  # Convert to absolute path so I don't end up with unnecessary nested
  # directories
  outdir <- normalizePath(outdir)
  settings$outdir <- outdir

  #Database BETY
  settings$database <- list(
    bety = list(user = db_bety_username,
                password = db_bety_password,
                host = db_bety_hostname,
                dbname = "bety",
                driver = db_bety_driver,
                write = FALSE),
    dbfiles = dbfiles_folder
  )

  #PFT
  if (is.null(pft)){
    # Select the first PFT in the model list.
    pft <- dplyr::tbl(con, "pfts") %>%
      dplyr::filter(.data$modeltype_id == !!model.new$modeltype_id) %>%
      dplyr::collect()
    pft <- pft$name[[1]]
    message("PFT is `NULL`. Defaulting to the following PFT: ",
            pft)
  }
  if (length(pft) > 1) {
    stop(
      "Currently, only a single PFT is supported. ",
      "Multiple PFTs will be implemented in a future version."
    )
  }
  settings$pfts <- list(
    pft = list(name = pft,
               constants = list(num = 1))
  )

  #Meta Analysis
  settings$meta.analysis <- list(iter = 3000, random.effects = FALSE)

  #Ensemble
  settings$ensemble <- list(
    size = ensemble_size,
    variable = sensitivity_variable,
    samplingspace = list(met = list(method = "sampling"),
                         parameters = list(method = "uniform"))
  )

  #Sensitivity
  if (sensitivity) {
    settings$sensitivity.analysis <- list(
      quantiles = list(sigma1 = -2, sigma2 = -1, sigma3 = 1, sigma4 = 2)
    )
  }

  #Model
  settings$model$id <- model.new[["id"]]

  #Workflow
  settings$workflow$id
  settings$workflow$id <- paste0("Test_run_","_",model.new$model_name)
  settings$run <- list(
    site = list(id = site_id, met.start = start_date, met.end = end_date),
    inputs = list(met = list(source = met, output = model.new[["model_name"]],
                             username = "pecan")),
    start.date = start_date, end.date = end_date
  )
  settings$host$name <- "localhost"

  #create file and Run
  XML::saveXML(PEcAn.settings::listToXml(settings, "pecan"),
	       file = file.path(outdir, "pecan.xml"))
  file.copy(file.path(pecan_path, "web", "workflow.R"), outdir)
  cwd <- getwd()
  setwd(outdir)
  on.exit(setwd(cwd), add = TRUE)

  sys_out <- system("Rscript workflow.R 2>&1 | tee workflow.Rout")

  list(
    sys = sys_out,
    outdir = outdir
  )
}
