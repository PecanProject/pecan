#' Run ED singularity container
#'
#' Uses [base::system2] to run ED or EDR via a Singularity container.
#'
#' On some systems, to run Singularity properly, you will need to bind 
#' additional paths. To do this, pass the arguments as a character vector to 
#' `singularity_args`. For instance:
#'
#' ```
#' bindpaths <- c("/scratch", "/data")
#' run_ed_singularity(..., singularity_args = paste("--bind", bindpaths))
#' ```
#'
#' By default, [base::system2] prints the output to the console. To store 
#' standard ED output in a variable as a character vector, set `stdout = TRUE`. 
#' To redirect all output to the variable, including GCC exceptions, use 
#' `stderr = TRUE` (this will automatically set `stdout = TRUE` as well). 
#' Output can also be redirected to a file via `stderr = "/path/to/file.log"`.
#'
#' @param img_path Path to Singularity container (usually a `.simg` file)
#' @param ed2in_path Path to ED2IN file.
#' @param app Singularity "app" to run. Either "ED" or "EDR".
#' @param singularity_args Additional arguments to be passed to `singularity run` (before)
#' @param Additional arguments to [base::system2]
#' @export
run_ed_singularity <- function(img_path, ed2in_path,
                               app = "ED",
                               singularity_args = NULL,
                               ...) {
  if (!file.exists(img_path)) {
    PEcAn.logger::logger.severe("Image file ", img_path, " not found.")
  }
  if (!file.exists(ed2in_path)) {
    PEcAn.logger::logger.severe("ED2IN file ", ed2in_path, " not found.")
  }
  sys_args <- c(
    "run",
    "--app", app,
    singularity_args,
    normalizePath(img_path, mustWork = TRUE),
    "-f",
    normalizePath(ed2in_path, mustWork = TRUE)
  )
  system2("singularity", sys_args, ...)
}
