##' Generate Validation Benchmark Report
##'
##' @param benchmark_results A list containing `metrics` (data.frame), `aligned_data` (data.frame), and `plots` (list of ggplot objects) returned by the validation pipeline.
##' @param output_file The path where the compiled report should be saved (e.g., "validation_report.html").
##' @param template The path to the Quarto template. Defaults to the one provided in the package `inst/reports/Validation_report.qmd`.
##' 
##' @author PEcAn Project
##' @export
generate_validation_report <- function(benchmark_results, output_file = "Validation_report.html", template = NULL) {
  PEcAn.logger::logger.info("Generating Validation Benchmark Report...")
  
  if (is.null(template)) {
    template <- system.file("reports", "Validation_report.qmd", package = "PEcAn.benchmark")
    if (template == "") {
      # Fallback for development mode
      template <- file.path(getwd(), "inst", "reports", "Validation_report.qmd")
    }
  }
  
  if (!file.exists(template)) {
    PEcAn.logger::logger.severe("Template file not found:", template)
    stop("Quarto template not found.")
  }
  
  if (!requireNamespace("quarto", quietly = TRUE)) {
    PEcAn.logger::logger.severe("The 'quarto' package is required to generate the report.")
    stop("Please install the 'quarto' R package.")
  }
  
  # Ensure absolute paths
  output_file <- normalizePath(output_file, mustWork = FALSE)
  output_dir <- dirname(output_file)
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Copy template to output directory to avoid permission issues in system folders
  temp_qmd <- file.path(output_dir, basename(template))
  file.copy(template, temp_qmd, overwrite = TRUE)
  
  # Render the document
  tryCatch({
    quarto::quarto_render(
      input = temp_qmd,
      output_file = basename(output_file),
      execute_params = list(benchmark_results = benchmark_results)
    )
    
    PEcAn.logger::logger.info("Validation report successfully generated at:", output_file)
  }, error = function(e) {
    PEcAn.logger::logger.severe("Failed to render validation report:", e$message)
    stop(e)
  }, finally = {
    # Clean up the temporary template file
    if (file.exists(temp_qmd)) {
      file.remove(temp_qmd)
    }
  })
  
  return(invisible(output_file))
}
