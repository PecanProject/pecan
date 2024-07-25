#' Get machine information from db
#' @param host host information
#' @param dbfile.id dbfile id for existing records
#' @param input.id input id for existing records
#' @param con database connection
#' 
#' @return list of machine, input, and dbfile records
#' @author Betsy Cowdery, Michael Dietze, Ankur Desai, Tony Gardella, Luke Dramko

get.machine.info <- function(host, dbfile.id, input.id = NULL, con) {
    machine.host <- ifelse(host$name == "localhost", PEcAn.remote::fqdn(), host$name)
    machine <- db.query(paste0(
        "SELECT * from machines where hostname = '",
        machine.host, "'"
    ), con)

    if (nrow(machine) == 0) {
        PEcAn.logger::logger.error("machine not found", host$name)
        return(NULL)
    }

    if (missing(input.id) || is.na(input.id) || is.null(input.id)) {
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
