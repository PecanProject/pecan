#' Return new arrangement of database while adding code to deal with ensembles
#'
#' @param result list of results from the download function
#' @param con database connection
#' @param start_date start date of the data
#' @param end_date end date of the data
#' @param write whether to write to the database
#' @param overwrite Logical: If a file already exists, create a fresh copy?
#' @param insert.new.file whether to insert a new file
#' @param input.args input arguments obtained from the convert_input function
#' @param machine machine information
#' @param mimetype data product specific file format
#' @param formatname format name of the data
#' @param allow.conflicting.dates whether to allow conflicting dates
#' @param ensemble ensemble id
#' @param ensemble_name ensemble name
#' @param existing.input existing input records
#' @param existing.dbfile existing dbfile records
#' @param input input records
#' @return list of input and dbfile ids
#'
#' @author Betsy Cowdery, Michael Dietze, Ankur Desai, Tony Gardella, Luke Dramko

add.database.entries <- function(
    result, con, start_date,
    end_date, write, overwrite,
    insert.new.file, input.args,
    machine, mimetype, formatname,
    allow.conflicting.dates, ensemble,
    ensemble_name, existing.input,
    existing.dbfile, input) {
    if (write) {
        # Setup newinput. This list will contain two variables: a vector of input IDs and a vector of DB IDs for each entry in result.
        # This list will be returned.
        newinput <- list(input.id = NULL, dbfile.id = NULL) # Blank vectors are null.
        for (i in 1:length(result)) { # Master for loop
            id_not_added <- TRUE

            if (!is.null(existing.input) && nrow(existing.input[[i]]) > 0 &&
                (existing.input[[i]]$start_date != start_date || existing.input[[i]]$end_date != end_date)) {
                # Updating record with new dates
                db.query(paste0("UPDATE inputs SET start_date='", start_date, "', end_date='", end_date, "' WHERE id=", existing.input[[i]]$id), con)
                id_not_added <- FALSE

                # The overall structure of this loop has been set up so that exactly one input.id and one dbfile.id will be written to newinput every iteration.
                newinput$input.id <- c(newinput$input.id, existing.input[[i]]$id)
                newinput$dbfile.id <- c(newinput$dbfile.id, existing.dbfile[[i]]$id)
            }

            if (overwrite) {
                # A bit hacky, but need to make sure that all fields are updated to expected values (i.e., what they'd be if convert_input was creating a new record)
                if (!is.null(existing.input) && nrow(existing.input[[i]]) > 0) {
                    db.query(paste0("UPDATE inputs SET name='", basename(dirname(result[[i]]$file[1])), "' WHERE id=", existing.input[[i]]$id), con)
                }

                if (!is.null(existing.dbfile) && nrow(existing.dbfile[[i]]) > 0) {
                    db.query(paste0("UPDATE dbfiles SET file_path='", dirname(result[[i]]$file[1]), "', file_name='", result[[i]]$dbfile.name[1], "' WHERE id=", existing.dbfile[[i]]$id), con)
                }
            }

            # If there is no ensemble then for each record there should be one parent
            # But when you have ensembles, all of the members have one parent !!
            parent.id <- if (is.numeric(ensemble)) {
                ifelse(is.null(input[[i]]), NA, input[[1]]$id)
            } else {
                ifelse(is.null(input[[i]]), NA, input[[i]]$id)
            }


            if ("newsite" %in% names(input.args) && !is.null(input.args[["newsite"]])) {
                site.id <- input.args$newsite
            }

            if (insert.new.file && id_not_added) {
                dbfile.id <- dbfile.insert(in.path = dirname(result[[i]]$file[1]), in.prefix = result[[i]]$dbfile.name[1], "Input", existing.input[[i]]$id, con, reuse = TRUE, hostname = machine$hostname)
                newinput$input.id <- c(newinput$input.id, existing.input[[i]]$id)
                newinput$dbfile.id <- c(newinput$dbfile.id, dbfile.id)
            } else if (id_not_added) {
                # This is to tell input.insert if we are writing ensembles
                # Why does it need it? Because it checks for inputs with the same time period, site, and machine
                # and if it returns something it does not insert anymore, but for ensembles, it needs to bypass this condition
                ens.flag <- if (!is.null(ensemble) | is.null(ensemble_name)) TRUE else FALSE

                new_entry <- dbfile.input.insert(
                    in.path = dirname(result[[i]]$file[1]),
                    in.prefix = result[[i]]$dbfile.name[1],
                    siteid = site.id,
                    startdate = start_date,
                    enddate = end_date,
                    mimetype = mimetype,
                    formatname = formatname,
                    parentid = parent.id,
                    con = con,
                    hostname = machine$hostname,
                    allow.conflicting.dates = allow.conflicting.dates,
                    ens = ens.flag
                )

                newinput$input.id <- c(newinput$input.id, new_entry$input.id)
                newinput$dbfile.id <- c(newinput$dbfile.id, new_entry$dbfile.id)
            }
        } # End for loop

        successful <- TRUE
        return(newinput)
    } else {
        PEcAn.logger::logger.warn("Input was not added to the database")
        successful <- TRUE
        return(NULL)
    }
}
