#' Insert or Update Database Records for New or Modified Input Data
#' 
#' @title Insert or Update Database Records for New or Modified Input Data
#' @description This function is called internally by [convert_input()] to insert or update **input** and **dbfile** records in the PEcAn BETY database after one or more data-conversion or download functions have produced local or remote files. It is specifically intended for use with the output from data-conversion functions called by [convert_input()] (e.g. various "download_X" or "met2model_X" functions), but can be adapted if the return structure matches the requirements below.
#' 
#' @param result list of data frames, each data frame corresponding to one piece or "chunk" of newly-created data. Typically, these data frames are produced by the function specified in `convert_input(..., fcn=...)`. Each data frame must contain at least: \describe{ \item{file}{Absolute file path(s) to the newly created file(s).} \item{dbfile.name}{The base filename(s) (without leading path) for each corresponding file.} } Additional columns are allowed but unused by this function.
#' @param con database connection object (as returned by, e.g., \code{\link[DBI]{dbConnect}}).
#' @param start_date Date or character. The start date of the data (in UTC). Acceptable types include Date objects (`as.Date`) or character strings that can be parsed to a Date via standard R conversions.
#' @param end_date Date or character. The end date of the data (in UTC). Acceptable types include Date objects (`as.Date`) or character strings that can be parsed to a Date via standard R conversions.
#' @param overwrite logical. If `TRUE`, any existing database records and files for the same input and date range should be overwritten with the new files. If `FALSE`, existing files are preserved.
#' @param insert.new.file logical. If `TRUE`, forces the creation of a new **dbfile** entry even if an existing entry is found. Typically used for forecast or ensemble data that may be partially present.
#' @param input.args list. This is passed from [convert_input()] and contains auxiliary arguments or settings that were passed along internally. It may include items such as `newsite` (integer site ID), among others. Its exact contents are not strictly defined but typically include the arguments provided to `convert_input()`.
#' @param machine data.frame. Single row describing the machine on which the new data resides. It typically has columns like `id` and `hostname`, indicating the corresponding row in BETY's `machines` table.
#' @param mimetype character. String indicating the file's MIME type (e.g. `"text/csv"`, `"application/x-netcdf"`, etc.).
#' @param formatname character. String describing the file format (as listed in BETYdb's `formats` table). For example `"CF Meteorology"`.
#' @param allow.conflicting.dates logical. If `TRUE`, allows creation or insertion of new file records even if their date range overlaps with existing records. If `FALSE`, overlapping ranges may cause errors or be disallowed.
#' @param ensemble integer or logical. If an integer > 1, indicates that multiple ensemble members were generated (often for forecast data) and that each member may need separate database entries. If `FALSE`, the data are not an ensemble.
#' @param ensemble_name character. String providing a descriptive label or identifier for an ensemble member. Typically used if `convert_input()` is called iteratively for each member.
#' @param existing.input data.frame. Possibly zero rows representing the current record(s) in the `inputs` table that match (or partially match) the data being added. If no matching record exists, an empty data frame is supplied.
#' @param existing.dbfile data.frame. Possibly zero rows representing the current record(s) in the `dbfiles` table that match (or partially match) the data being added. If no matching record exists, an empty data frame is supplied.
#' @param input data.frame. Single row with the parent input record from BETYdb, typically including columns like `id`, `start_date`, `end_date`, etc. If the new data are derived from an existing input, this links them in the `parent_id` column of the new entries.
#' 
#' @return list with two elements: \describe{ \item{input.id}{A numeric vector of new (or updated) input record IDs.} \item{dbfile.id}{A numeric vector of new (or updated) dbfile record IDs.} }
#' 
#' @details This function consolidates the final step of adding or updating records in the BETY database to reflect newly created data files. It either updates existing `input` and `dbfile` records or creates new records, depending on the provided arguments (`overwrite`, `insert.new.file`, etc.) and whether a matching record already exists. Typically, these records represent model-ready meteorological or other environmental data, after format conversion or downloading has taken place in [convert_input()].
#' 
#' @author Betsy Cowdery, Michael Dietze, Ankur Desai, Tony Gardella, Luke Dramko

update_ensemble_writes <- function(
    result, con, start_date,
    end_date, overwrite,
    insert.new.file, input.args,
    machine, mimetype, formatname,
    allow.conflicting.dates, ensemble,
    ensemble_name, existing.input,
    existing.dbfile, input) {
    # Setup newinput. This list will contain two variables: a vector of input IDs and a vector of DB IDs for each entry in result.
    # This list will be returned.
    newinput <- list(input.id = NULL, dbfile.id = NULL) # Blank vectors are null.

    for (i in seq_along(result)) { # Master for loop
        id_not_added <- TRUE

        if (!is.null(existing.input) && nrow(existing.input[[i]]) > 0 &&
            (existing.input[[i]]$start_date != start_date || existing.input[[i]]$end_date != end_date)) {
            # Updating record with new dates
            db.query(
                paste0(
                    "UPDATE inputs SET start_date='", start_date, "', end_date='", end_date,
                    "' WHERE id=", existing.input[[i]]$id
                ),
                con
            )
            id_not_added <- FALSE

            # The overall structure of this loop has been set up so that exactly one input.id and one dbfile.id will be written to newinput every iteration.
            newinput$input.id <- c(newinput$input.id, existing.input[[i]]$id)
            newinput$dbfile.id <- c(newinput$dbfile.id, existing.dbfile[[i]]$id)
        }

        if (overwrite) {
            # A bit hacky, but need to make sure that all fields are updated to expected values (i.e., what they'd be if convert_input was creating a new record)
            if (!is.null(existing.input) && nrow(existing.input[[i]]) > 0) {
                db.query(
                    paste0(
                        "UPDATE dbfiles SET file_path='", dirname(result[[i]]$file[1]),
                        "', file_name='", result[[i]]$dbfile.name[1],
                        "' WHERE id=", existing.dbfile[[i]]$id
                    ),
                    con
                )
            }

            if (!is.null(existing.dbfile) && nrow(existing.dbfile[[i]]) > 0) {
                db.query(paste0(
                    "UPDATE dbfiles SET file_path='", dirname(result[[i]]$file[1]),
                    "', file_name='", result[[i]]$dbfile.name[1],
                    "' WHERE id=", existing.dbfile[[i]]$id
                ), con)
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
            dbfile.id <- dbfile.insert(
                in.path = dirname(result[[i]]$file[1]),
                in.prefix = result[[i]]$dbfile.name[1],
                "Input",
                existing.input[[i]]$id,
                con,
                reuse = TRUE,
                hostname = machine$hostname
            )

            newinput$input.id <- c(newinput$input.id, existing.input[[i]]$id)
            newinput$dbfile.id <- c(newinput$dbfile.id, dbfile.id)
        } else if (id_not_added) {
            # This is to tell input.insert if we are writing ensembles
            # Why does it need it? Because it checks for inputs with the same time period, site, and machine
            # and if it returns something it does not insert anymore, but for ensembles, it needs to bypass this condition
            ens.flag <- if (!is.null(ensemble) || is.null(ensemble_name)) TRUE else FALSE

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
    return(newinput)
}
