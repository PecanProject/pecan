##' @export cull_database_entries
##' @author Tempest McCabe
##'
##' @param outdir  Directory from which the file will be read, and where the delete_log_FILE_NAME will be read to
##' @param file_name The name of the file being read in
##' @param con connection the the bety database
##' @param machine_id Optional id of the machine that contains the bety entries.
##'
##' @description This is a fucntion that takes in a table of records and deletes everything in the file. Please do not run this function without
##' 1) Backing Up Bety
##' 2) Checking the the file only contains entries to be deleted.
##'
##' For more information on how to use this function see the "Pre-release-database-cleanup" script in the 'vignettes' folder
##' or look at the README
##'
##'

cull_database_entries <- function(table = NULL, outdir, file_name = NULL, con, machine_id = NULL, table_name = NULL) {
  if (is.null(table)) {
    if (is.null(file_name)) {
      PEcAn.logger::logger.severe("If a table object hasn't been provided, a file_name must be set.")
    } else {
      file <- paste(outdir, "/", file_name, sep = "")
      table <- read.table(file = file, header = TRUE, sep = "|") # '|' chosen because it's unlikely to overlap with "notes" text.
    }
  } else if (!is.null(table) && !is.null(file_name)) {
    PEcAn.logger::logger.severe("table and file_name cannot both be provided. Providing table prevents file_name from being read in. Please choose one to avoid acidential deletions.")
  } else {
    if (!is.null(table_name)) {
      table$table_name <- rep(table_name, length(table$id))
    } else {
      PEcAn.logger::logger.severe("Please provide a table_name")
    }
  }

  if (!"table_name" %in% names(table)) {
    PEcAn.logger::logger.severe("Input file needs a 'table_name' column. Please check the file and the function that generated it.")
  }

  if ("dbfile" %in% table$table_name) {
    if (!is.null(machine_id) && "machine_id" %in% colnames(table)) {
      table <- table[table$machine_id == machine_id] # prevents deletion of files from other databases
    } else {
      PEcAn.logger::logger.info("Either machine_id is set to NULL or machine_id isn't a column name in table. No subssetting by machine will occur. ")
    }
  }

  if (is.null(file_name)) {
    file_name <- table_name
  }

  log <- list()
  for (i in seq_along(table$id)) {
    table_name <- as.character(table$table_name[i])
    id <- table$id[i]

    select_command <- paste("select * from ", table_name, " where id = ", id, ";", sep = "")
    log[i] <- PEcAn.DB::db.query(query = select_command, con = con)

    delete_command <- paste("DELETE from ", table_name, " where id = ", id, ";", sep = "")
    PEcAn.DB::db.query(delete_command, con = con)
  }
  write.table(log, file = paste(outdir, "/deletion_log_of_", file_name, sep = ""), row.names = FALSE)
}
