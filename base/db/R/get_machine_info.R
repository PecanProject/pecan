#' Get machine information from db
#' @param host host information
#' @param input.args input args for existing records
#' @param input.id input id for existing records
#' @param con database connection
#'
#' @return list of machine, input, and dbfile records
#' @author Betsy Cowdery, Michael Dietze, Ankur Desai, Tony Gardella, Luke Dramko

get_machine_info <- function(host, input.args, input.id = NULL, con = NULL) {

  machine.host.info <- get_machine_host(host, con = con)
  machine.host <- machine.host.info$machine.host
  machine <- machine.host.info$machine

  if (nrow(machine) == 0) {
    PEcAn.logger::logger.error("machine not found", host$name)
    return(NULL)
  }

  if (is.na(input.id) || is.null(input.id)) {
    input <- dbfile <- NULL
  } else {
    input <- db.query(paste("SELECT * from inputs where id =", input.id), con)
    if (nrow(input) == 0) {
      PEcAn.logger::logger.error("input not found", input.id)
      return(NULL)
    }

    if (!is.null(input.args$dbfile.id)) {
      dbfile <-
        db.query(
          paste(
            "SELECT * from dbfiles where id=", input.args$dbfile.id, " and container_id =",
            input.id,
            " and container_type = 'Input' and machine_id =",
            machine$id
          ),
          con
        )
    } else {
      dbfile <-
        db.query(
          paste(
            "SELECT * from dbfiles where container_id =",
            input.id,
            " and container_type = 'Input' and machine_id =",
            machine$id
          ),
          con
        )
    }



    if (nrow(dbfile) == 0) {
      PEcAn.logger::logger.error("dbfile not found", input.id)
      return(NULL)
    }
    if (nrow(dbfile) > 1) {
      PEcAn.logger::logger.warn("multiple dbfile records, using last", dbfile)
      dbfile <- dbfile[nrow(dbfile), ]
    }
  }

  return(list(machine = machine, input = input, dbfile = dbfile))
}

#' Helper Function to retrieve machine host and machine informations
#' @param host host information
#' @param con database connection
#' @return list of machine host and machine information
#' @author Abhinav Pandey
get_machine_host <- function(host, con) {
  #Grab machine info of host machine
  machine.host <- ifelse(host$name == "localhost", PEcAn.remote::fqdn(), host$name)
  machine <- db.query(paste0(
    "SELECT * from machines where hostname = '",
    machine.host, "'"
  ), con)

  list(machine.host = machine.host, machine = machine)
}

check_and_handle_existing_files <- function(existing.dbfile, host, con, existing.input, start_date, end_date) {
  # Grab machine info of file that exists
  existing.machine <- db.query(paste0("SELECT * from machines where id = '",
                                        existing.dbfile$machine_id, "'"), con)

  # Grab machine info of host machine
  machine.host.info <- get_machine_host(host, con = con)
  machine.host <- machine.host.info$machine.host
  machine <- machine.host.info$machine

  if (existing.machine$id != machine$id) {
    PEcAn.logger::logger.info("Valid Input record found that spans desired dates, but valid files do not exist on this machine.")
    PEcAn.logger::logger.info("Downloading all years of Valid input to ensure consistency")
    return(list(insert.new.file = TRUE, start_date = existing.input$start_date, end_date = existing.input$end_date))
  } else {
    # There's an existing input that spans desired start/end dates with files on this machine
    PEcAn.logger::logger.info("Skipping this input conversion because files are already available.")
    return(list(input.id = existing.input$id, dbfile.id = existing.dbfile$id))
  }
}