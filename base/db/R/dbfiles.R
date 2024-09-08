##' Function to insert a file into the dbfiles table as an input
##'
##' This will write into the dbfiles, inputs, machines and formats the required
##' data to store the file
##' @name dbfile.input.insert
##' @title Insert file into tables
##' @param in.path path to the directory containing the file to be inserted
##' @param in.prefix initial portion of the filename that does not vary by date. Does not include directory; specify that as part of in.path
##' @param siteid the id of the site that this data is applicable to
##' @param startdate the start date of the data stored in the file
##' @param enddate the end date of the data stored in the file
##' @param mimetype the mime-type of the file
##' @param formatname the name of the format to distinguish between simmilair mime-types
##' @param parentid the id of the parent of the input
##' @param con database connection object
##' @param hostname the name of the host where the file is stored, this will default to the name of the current machine
##' @param allow.conflicting.dates Whether to allow a new input record with same siteid, name, and format but different start/end dates
##' @param ens In case of ensembles we could let to have more than one file associated with one input.
##' @return data.frame with the id, filename and pathname of the input that is requested
##' @export
##' @author Rob Kooper, Betsy Cowdery
##' @examples
##' \dontrun{
##'   dbfile.input.insert(
##'     in.path = 'trait.data.Rdata',
##'     in.prefix = siteid,
##'     startdate = startdate,
##'     enddate = enddate,
##'     mimetype = 'application/x-RData',
##'     formatname = 'traits',
##'     con = dbcon)
##' }
dbfile.input.insert <- function(in.path, in.prefix, siteid, startdate, enddate, mimetype, formatname,
                                parentid = NA, con, hostname = PEcAn.remote::fqdn(), allow.conflicting.dates = FALSE, ens = FALSE) {
  name <- basename(in.path)
  hostname <- default_hostname(hostname)

  # find mimetype, if it does not exist, it will create one
  mimetypeid <- get.id("mimetypes", "type_string", mimetype, con, create = TRUE)

  # find appropriate format, create if it does not exist
  formatid <- get.id(
    table = "formats",
    colnames = c("mimetype_id", "name"),
    values = c(mimetypeid, formatname),
    con = con,
    create = TRUE,
    dates = TRUE
  )


  # setup parent part of query if specified
  if (is.na(parentid)) {
    parent <- ""
  } else {
    parent <- paste0(" AND parent_id=", parentid)
  }

  # find appropriate input, if not in database, insert new input
  existing.input <- db.query(
    query = paste0(
      "SELECT * FROM inputs WHERE site_id=", siteid,
      " AND name= '", name,
      "' AND format_id=", formatid,
      parent, ";"
    ),
    con = con
  )

  inputid <- NULL
  if (nrow(existing.input) > 0) {
    # Convert dates to Date objects and strip all time zones (DB values are timezone-free)
    if (!is.null(startdate)) {
      startdate <- lubridate::force_tz(time = lubridate::as_date(startdate), tzone = "UTC")
    }
    if (!is.null(enddate)) {
      enddate <- lubridate::force_tz(time = lubridate::as_date(enddate), tzone = "UTC")
    }
    existing.input$start_date <- lubridate::force_tz(time = lubridate::as_date(existing.input$start_date), tzone = "UTC")
    existing.input$end_date <- lubridate::force_tz(time = lubridate::as_date(existing.input$end_date), tzone = "UTC")

    for (i in seq_len(nrow(existing.input))) {
      existing.input.i <- existing.input[i, ]
      if (is.na(existing.input.i$start_date) && is.null(startdate)) {
        inputid <- existing.input.i[["id"]]
      } else if (existing.input.i$start_date == startdate && existing.input.i$end_date == enddate) {
        inputid <- existing.input.i[["id"]]
      }
    }

    if (is.null(inputid) && !allow.conflicting.dates) {
      print(existing.input, digits = 10)
      PEcAn.logger::logger.error(paste0(
        "Duplicate inputs (in terms of site_id, name, and format_id) with differing ",
        "start/end dates are not allowed. The existing input record printed above would ",
        " conflict with the one to be inserted, which has requested start/end dates of ",
        startdate, "/", enddate, "Please resolve this conflict or set",
        "allow.conflicting.dates=TRUE if you want to allow multiple input records ",
        " with different dates."
      ))
      return(NULL)
    }
  }

  if (is.null(inputid)) {
    # Either there was no existing input, or there was but the dates don't match and
    # allow.conflicting.dates==TRUE. So, insert new input record.
    # adding is.null(startdate) to add inputs like soil that don't have dates
    if (parent == "" && is.null(startdate)) {
      cmd <- paste0(
        "INSERT INTO inputs ",
        "(site_id, format_id, name) VALUES (",
        siteid, ", ", formatid, ", '", name,
        "'", ") RETURNING id;"
      )
    } else if (parent == "" && !is.null(startdate)) {
      cmd <- paste0(
        "INSERT INTO inputs ",
        "(site_id, format_id, start_date, end_date, name) VALUES (",
        siteid, ", ", formatid, ", '", startdate, "', '", enddate, "','", name,
        "') RETURNING id;"
      )
    } else if (is.null(startdate)) {
      cmd <- paste0(
        "INSERT INTO inputs ",
        "(site_id, format_id, name, parent_id) VALUES (",
        siteid, ", ", formatid, ", '", name, "',", parentid, ") RETURNING id;"
      )
    } else {
      cmd <- paste0(
        "INSERT INTO inputs ",
        "(site_id, format_id, start_date, end_date, name, parent_id) VALUES (",
        siteid, ", ", formatid, ", '", startdate, "', '", enddate, "','", name, "',", parentid, ") RETURNING id;"
      )
    }
    # This is the id that we just registered
    inserted.id <- db.query(query = cmd, con = con)
    name.s <- name

    if (is.null(startdate)) {
      inputid <- db.query(
        query = paste0(
          "SELECT id FROM inputs WHERE site_id=", siteid,
          " AND format_id=", formatid, ";"
        ),
        con = con
      )$id
    } else {
      inputid <- db.query(
        query = paste0(
          "SELECT id FROM inputs WHERE site_id=", siteid,
          " AND format_id=", formatid,
          " AND start_date='", startdate,
          "' AND end_date='", enddate,
          "'", parent, ";"
        ),
        con = con
      )$id
    }
  } else {
    inserted.id <- data.frame(id = inputid) # in the case that inputid is not null then this means that there was an exsiting input
  }

  if (length(inputid) > 1 && !ens) {
    PEcAn.logger::logger.warn(paste0(
      "Multiple input files found matching parameters format_id = ", formatid,
      ", startdate = ", startdate, ", enddate = ", enddate, ", parent = ", parent, ".  Selecting the",
      " last input file.  This is normal for when an entire ensemble is inserted iteratively, but ",
      " is likely an error otherwise."
    ))
    inputid <- inputid[length(inputid)]
  } else if (ens) {
    inputid <- inserted.id$id
  }

  # find appropriate dbfile, if not in database, insert new dbfile
  dbfile <- dbfile.check(type = "Input", container.id = inputid, con = con, hostname = hostname)

  if (nrow(dbfile) > 0 & !ens) {
    if (nrow(dbfile) > 1) {
      print(dbfile)
      PEcAn.logger::logger.warn("Multiple dbfiles found. Using last.")
      dbfile <- dbfile[nrow(dbfile), ]
    }

    if (dbfile$file_name != in.prefix || dbfile$file_path != in.path && !ens) {
      print(dbfile, digits = 10)
      PEcAn.logger::logger.error(paste0(
        "The existing dbfile record printed above has the same machine_id and container ",
        "but a diferent file name than expected (prefix='", in.prefix, "', path=", in.path, ").",
        "This is not allowed."
      ))
      dbfileid <- NA
    } else {
      dbfileid <- dbfile[["id"]]
    }
  } else {
    # insert dbfile & return dbfile id
    dbfileid <- dbfile.insert(
      in.path = in.path, in.prefix = in.prefix, type = "Input", id = inputid,
      con = con, reuse = TRUE, hostname = hostname
    )
  }

  invisible(list(input.id = inputid, dbfile.id = dbfileid))
}

##' Function to check to see if a file exists in the dbfiles table as an input
##'
##' This will check the dbfiles, inputs, machines and formats tables to see if the
##' file exists
##' @name dbfile.input.check
##' @title Check for a file in the input/dbfiles tables
##' @param siteid the id of the site that this data is applicable to
##' @param startdate the start date of the data stored in the file
##' @param enddate the end date of the data stored in the file
##' @param mimetype the mime-type of the file
##' @param formatname the name of the format to distinguish between simmilair mime-types
##' @param parentid the id of the parent of the input
##' @param con database connection object
##' @param hostname the name of the host where the file is stored, this will default to the name of the current machine
##' @param exact.dates setting to include start and end date in input query
##' @param pattern text to seach for in the file name (default NULL = no check).
##' @param return.all (Logical) If 'TRUE', return all files. If 'FALSE', return only the most recent files.
##' @return data.frame with the id, filename and pathname of the input that is requested
##' @export
##' @author Rob Kooper, Tony Gardella, Hamze Dokoohaki
##' @examples
##' \dontrun{
##'   dbfile.input.check(siteid, startdate, enddate, 'application/x-RData', 'traits', dbcon)
##' }
dbfile.input.check <- function(siteid, startdate = NULL, enddate = NULL, mimetype, formatname, parentid = NA,
                               con, hostname = PEcAn.remote::fqdn(), exact.dates = FALSE, pattern = NULL, return.all = FALSE) {
  hostname <- default_hostname(hostname)

  mimetypeid <- get.id(table = "mimetypes", colnames = "type_string", values = mimetype, con = con)
  if (is.null(mimetypeid)) {
    return(invisible(data.frame()))
  }

  # find appropriate format
  formatid <- get.id(table = "formats", colnames = c("mimetype_id", "name"), values = c(mimetypeid, formatname), con = con)

  if (is.null(formatid)) {
    invisible(data.frame())
  }

  # setup parent part of query if specified
  if (is.na(parentid)) {
    parent <- ""
  } else {
    parent <- paste0(" AND parent_id=", parentid)
  }

  # find appropriate input
  if (exact.dates) {
    if (!is.null(enddate)) {
      inputs <- db.query(
        query = paste0(
          "SELECT * FROM inputs WHERE site_id=", siteid,
          " AND format_id=", formatid,
          " AND start_date='", startdate,
          "' AND end_date='", enddate,
          "'", parent
        ),
        con = con
      )
    } else {
      inputs <- db.query(
        query = paste0(
          "SELECT * FROM inputs WHERE site_id=", siteid,
          " AND format_id=", formatid,
          " AND start_date='", startdate,
          "'", parent
        ),
        con = con
      )
    }
  } else { # not exact dates
    inputs <- db.query(
        query = paste0(
          "SELECT * FROM inputs WHERE site_id=", siteid,
          " AND format_id=", formatid,
          parent
        ),
        con = con
    )
  }

  if (is.null(inputs) | length(inputs$id) == 0) {
    return(data.frame())
  } else {
    if (!is.null(pattern)) {
      ## Case where pattern is not NULL
      inputs <- inputs[grepl(pattern, inputs$name), ]
    }

    ## parent check when NA
    # if (is.na(parentid)) {
    #   inputs <- inputs[is.na(inputs$parent_id),]
    # }

    if (length(inputs$id) > 1) {
      PEcAn.logger::logger.warn("Found multiple matching inputs. Checking for one with associate files on host machine")
      print(inputs)
      #   ni = length(inputs$id)
      #   dbfile = list()
      #   for(i in seq_len(ni)){
      #     dbfile[[i]] <- dbfile.check(type = 'Input', container.id = inputs$id[i], con = con, hostname = hostname, machine.check = TRUE)
      # }

      dbfile <-
        dbfile.check(
          type = "Input",
          container.id = inputs$id,
          con = con,
          hostname = hostname,
          machine.check = TRUE,
          return.all = return.all
        )


      if (nrow(dbfile) == 0) {
        ## With the possibility of dbfile.check returning nothing,
        ## as.data.frame ensures a empty data.frame is returned
        ## rather than an empty list.
        PEcAn.logger::logger.info("File not found on host machine. Returning Valid input with file associated on different machine if possible")
        return(as.data.frame(dbfile.check(type = "Input", container.id = inputs$id, con = con, hostname = hostname, machine.check = FALSE)))
      }

      return(dbfile)
    } else if (length(inputs$id) == 0) {

      # need this third case here because prent check above can return an empty inputs
      return(data.frame())
    } else {
      PEcAn.logger::logger.warn("Found possible matching input. Checking if its associate files are on host machine")
      print(inputs)
      dbfile <- dbfile.check(
        type = "Input",
        container.id = inputs$id,
        con = con,
        hostname = hostname,
        machine.check = TRUE,
        return.all = return.all
      )

      if (nrow(dbfile) == 0) {
        ## With the possibility of dbfile.check returning nothing,
        ## as.data.frame ensures an empty data.frame is returned
        ## rather than an empty list.
        PEcAn.logger::logger.info("File not found on host machine. Returning Valid input with file associated on different machine if possible")
        return(as.data.frame(dbfile.check(type = "Input", container.id = inputs$id, con = con, hostname = hostname, machine.check = FALSE)))
      }

      return(dbfile)
    }
  }
}

##' Function to insert a file into the dbfiles table as a posterior
##'
##' This will write into the dbfiles, posteriors, machines and formats the require
##' data to store the file
##' @name dbfile.posterior.insert
##' @title Insert file into tables
##' @param filename the name of the file to be inserted
##' @param pft the name of the pft that this data is applicable to
##' @param mimetype the mime-type of the file
##' @param formatname the name of the format to distinguish between simmilair mime-types
##' @param con database connection object
##' @param hostname the name of the host where the file is stored, this will default to the name of the current machine
##' @return data.frame with the id, filename and pathname of the posterior that is requested
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##'   dbfile.posterior.insert('trait.data.Rdata', pft, 'application/x-RData', 'traits', dbcon)
##' }
dbfile.posterior.insert <- function(filename, pft, mimetype, formatname, con, hostname = PEcAn.remote::fqdn()) {
  hostname <- default_hostname(hostname)

  # find appropriate pft
  pftid <- get.id("pfts", "name", pft, con)
  if (is.null(pftid)) {
    PEcAn.logger::logger.severe("Could not find pft, could not store file", filename)
  }

  mimetypeid <- get.id(
    table = "mimetypes", colnames = "type_string", values = mimetype,
    con = con, create = TRUE
  )

  # find appropriate format
  formatid <- get.id(
    table = "formats", colnames = c("mimetype_id", "name"), values = c(mimetypeid, formatname),
    con = con, create = TRUE, dates = TRUE
  )

  # find appropriate posterior
  # NOTE: This is defined but not used
  # posterior_ids <- get.id("posteriors", "pft_id", pftid, con)

  posteriorid_query <- paste0(
    "SELECT id FROM posteriors WHERE pft_id=", pftid,
    " AND format_id=", formatid
  )
  posteriorid <- db.query(query = posteriorid_query, con = con)[["id"]]
  if (is.null(posteriorid)) {
    # insert input
    db.query(
      query = paste0(
        "INSERT INTO posteriors (pft_id, format_id)",
        " VALUES (", pftid, ", ", formatid, ")"
      ),
      con = con
    )
    posteriorid <- db.query(posteriorid_query, con)[["id"]]
  }

  # NOTE: Modified by Alexey Shiklomanov.
  # I'm not sure how this is supposed to work, but I think it's like this
  invisible(dbfile.insert(
    in.path = dirname(filename), in.prefix = basename(filename), type = "Posterior", id = posteriorid,
    con = con, reuse = TRUE, hostname = hostname
  ))
}

##' Function to check to see if a file exists in the dbfiles table as an input
##'
##' This will check the dbfiles, inputs, machines and formats tables to see if the
##' file exists
##' @name dbfile.posterior.check
##' @title Check for a file in the input/dbfiles tables
##' @param pft the name of the pft that this data is applicable to
##' @param mimetype the mime-type of the file
##' @param formatname the name of the format to distinguish between simmilair mime-types
##' @param con database connection object
##' @param hostname the name of the host where the file is stored, this will default to the name of the current machine
##' @return data.frame with the id, filename and pathname of the posterior that is requested
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##'   dbfile.posterior.check(pft, 'application/x-RData', 'traits', dbcon)
##' }
dbfile.posterior.check <- function(pft, mimetype, formatname, con, hostname = PEcAn.remote::fqdn()) {
  hostname <- default_hostname(hostname)

  # find appropriate pft
  pftid <- get.id(table = "pfts", values = "name", colnames = pft, con = con)
  if (is.null(pftid)) {
    invisible(data.frame())
  }

  # find appropriate format
  mimetypeid <- get.id(table = "mimetypes", values = "type_string", colnames = mimetype, con = con)
  if (is.null(mimetypeid)) {
    PEcAn.logger::logger.error("mimetype ", mimetype, "does not exist")
  }
  formatid <- get.id(table = "formats", colnames = c("mimetype_id", "name"), values = c(mimetypeid, formatname), con = con)

  if (is.null(formatid)) {
    invisible(data.frame())
  }

  # find appropriate posterior
  posteriorid <- db.query(
    query = paste0(
      "SELECT id FROM posteriors WHERE pft_id=", pftid,
      " AND format_id=", formatid
    ),
    con = con
  )[["id"]]
  if (is.null(posteriorid)) {
    invisible(data.frame())
  }

  invisible(dbfile.check(type = "Posterior", container.id = posteriorid, con = con, hostname = hostname))
}

##' Function to insert a file into the dbfiles table
##'
##' This will write into the dbfiles and machines the required data to store the file
##' @name dbfile.insert
##' @title Insert file into tables
##' @param in.path Path to file directory
##' @param in.prefix Filename prefix (not including directory)
##' @param type One of "Model", "Posterior", "Input"
##' @param id container_id of the input to be modified
##' @param reuse logical: If a record already exists, use it or create a new one?
##' @param con database connection object
##' @param hostname the name of the host where the file is stored, this will default to the name of the current machine
##' @return id of the file that is written
##' @author Rob Kooper, Ryan Kelly
##' @export
##' @examples
##' \dontrun{
##'   dbfile.insert('somefile.txt', 'Input', 7, dbcon)
##' }
dbfile.insert <- function(in.path, in.prefix, type, id, con, reuse = TRUE, hostname = PEcAn.remote::fqdn()) {
  hostname <- default_hostname(hostname)

  if (substr(in.path, 1, 1) != "/") {
    PEcAn.logger::logger.error("path to dbfiles:", in.path, " is not a valid full path")
  }

  # find appropriate host
  hostid <- get.id(table = "machines", colnames = "hostname", values = hostname, con = con, create = TRUE, dates = TRUE)

  # Query for existing dbfile record with same file_name, file_path, machine_id ,
  # container_type, and container_id.
  dbfile <- invisible(db.query(
    query = paste0(
      "SELECT * FROM dbfiles WHERE ",
      "file_name='", basename(in.prefix), "' AND ",
      "file_path='", in.path, "' AND ",
      "machine_id='", hostid, "'"
    ),
    con = con
  ))

  if (nrow(dbfile) == 0) {
    # If no existing record, insert one

    insert_result <- db.query(
      query = paste0(
        "INSERT INTO dbfiles ",
        "(container_type, container_id, file_name, file_path, machine_id) VALUES (",
        "'", type, "', ", id, ", '", basename(in.prefix), "', '", in.path, "', ", hostid,
        ") RETURNING id"
      ),
      con = con
    )

    file.id <- insert_result[["id"]]
  } else if (!reuse) {
    # If there is an existing record but reuse==FALSE, return NA.
    file.id <- NA
  } else {
    if (dbfile$container_type != type || dbfile$container_id != id) {
      print(dbfile, digits = 10)
      PEcAn.logger::logger.error(paste0(
        "The existing dbfile record printed above has the same machine_id, file_path, and file_name ",
        "but is associated with a different input than requested (type='", type, "', id=", id, ").",
        "This is not allowed."
      ))
      file.id <- NA
    } else {
      file.id <- dbfile[["id"]]
    }
  }

  # Return the new dbfile ID, or the one that existed already (reuse==T), or NA (reuse==F)
  return(file.id)
}

##' List files associated with a container and machine exist in
##' `dbfiles` table
##'
##' @param type The type of `dbfile`, as a character. Must be either
##'   "Input", "Posterior", or "Model".
##' @param container.id the ID of container type. E.g. if `type` is
##'   "Input", this will correspond to the `id` column from the
##'   `inputs` table.
##' @param con database connection object
##' @param hostname the name of the host where the file is stored,
##'   this will default to the name of the current machine
##' @param machine.check setting to check for file on named host,
##'   otherwise will check for any file given container id
##' @param return.all (Logical) If `TRUE`, return all files. If
##'   `FALSE`, return only the most recent files.
##' @return `data.frame` with the id, filename and pathname of all the files that are associated
##' @author Rob Kooper, Alexey Shiklomanov
##' @export
##' @examples
##' \dontrun{
##'   dbfile.check("Input", 7, dbcon)
##' }
dbfile.check <- function(type, container.id, con,
                         hostname = PEcAn.remote::fqdn(),
                         machine.check = TRUE,
                         return.all = FALSE) {
  type <- match.arg(type, c("Input", "Posterior", "Model"))

  hostname <- default_hostname(hostname)

  # find appropriate host
  hostid <- get.id(table = "machines", colnames = "hostname", values = hostname, con = con)
  if (is.null(hostid)) {
    return(data.frame())
  }

  dbfiles <- dplyr::tbl(con, "dbfiles") %>%
    dplyr::filter(
      .data$container_type == !!type,
      .data$container_id %in% !!container.id
    )

  if (machine.check) {
    dbfiles <- dbfiles %>%
      dplyr::filter(.data$machine_id == !!hostid)
  }

  dbfiles <- dplyr::collect(dbfiles)

  if (nrow(dbfiles) > 1 && !return.all) {
    PEcAn.logger::logger.warn("Multiple Valid Files found on host machine. Returning last updated record.")
    dbfiles <- dbfiles %>%
      dplyr::filter(.data$updated_at == max(.data$updated_at))
  }

  dbfiles
}

##' Convert between file paths and ids
##'
##' These functions check the dbfiles and machines tables to see if the file
##' exists, and return the container_id (\code{dbfile.id}) or full filename
##' with path (\code{dbfile.file}) to the first one found.
##' If none is found, both will return NA.
##'
##' @describeIn dbfile.file Return full path to file from the dbfiles table
##' @param type the type of dbfile (Input, Posterior)
##' @param id the id of container type
##' @param con database connection object
##' @param hostname the name of the host where the file is stored, this will default to the name of the current machine
##' @return filename on host, or NA if none found
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##'   dbfile.file('Input', 7, dbcon)
##' }
dbfile.file <- function(type, id, con, hostname = PEcAn.remote::fqdn()) {
  hostname <- default_hostname(hostname)

  files <- dbfile.check(type = type, container.id = id, con = con, hostname = hostname)

  if (nrow(files) > 1) {
    PEcAn.logger::logger.warn("multiple files found for", id, "returned; using the first one found")
    invisible(file.path(files[1, "file_path"], files[1, "file_name"]))
  } else if (nrow(files) == 1) {
    invisible(file.path(files[1, "file_path"], files[1, "file_name"]))
  } else {
    PEcAn.logger::logger.warn("no files found for ", id, "in database")
    invisible(NA)
  }
}

##' @describeIn dbfile.file Return id to container type given a filename
##' @param file the full pathname to the file
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##'   dbfile.id('Model', '/usr/local/bin/sipnet', dbcon)
##' }
dbfile.id <- function(type, file, con, hostname = PEcAn.remote::fqdn()) {
  hostname <- default_hostname(hostname)

  # find appropriate host
  hostid <- db.query(query = paste0("SELECT id FROM machines WHERE hostname='", hostname, "'"), con = con)[["id"]]
  if (is.null(hostid)) {
    invisible(NA)
  }

  # find file
  file_name <- basename(file)
  file_path <- dirname(file)
  ids <- db.query(
    query = paste0(
      "SELECT container_id FROM dbfiles WHERE container_type='", type,
      "' AND file_path='", file_path,
      "' AND file_name='", file_name,
      "' AND machine_id=", hostid
    ),
    con = con
  )

  if (nrow(ids) > 1) {
    PEcAn.logger::logger.warn("multiple ids found for", file, "returned; using the first one found")
    invisible(ids[1, "container_id"])
  } else if (nrow(ids) == 1) {
    invisible(ids[1, "container_id"])
  } else {
    PEcAn.logger::logger.warn("no id found for", file, "in database")
    invisible(NA)
  }
}



##'
##' This function will move dbfiles - clim or nc -  from one location
##' to another on the same machine and update BETY
##'
##' @name dbfile.move
##' @title Move files to new location
##' @param old.dir directory with files to be moved
##' @param new.dir directory where files should be moved
##' @param file.type what type of files are being moved
##' @param siteid needed to register files that arent already in BETY
##' @param register if file isn't already in BETY, should it be registered?
##' @return print statement of how many files were moved, registered, or have symbolic links
##' @export
##' @author kzarada
##' @examples
##' \dontrun{
##'   dbfile.move(
##'   old.dir = "/fs/data3/kzarada/pecan.data/dbfiles/NOAA_GEFS_site_0-676",
##'   new.dir = '/projectnb/dietzelab/pecan.data/dbfiles/NOAA_GEFS_site_0-676'
##'   file.type= clim,
##'   siteid = 676,
##'   register = TRUE
##'   )
##' }


dbfile.move <- function(old.dir, new.dir, file.type, siteid = NULL, register = FALSE) {


  # create nulls for file movement and error info
  error <- 0
  files.sym <- 0
  files.changed <- 0
  files.reg <- 0
  files.indb <- 0

  # check for file type and update to make it *.file type
  if (file.type != "clim" | file.type != "nc") {
    PEcAn.logger::logger.error("File type not supported by move at this time. Currently only supports NC and CLIM files")
    error <- 1
  }
  file.pattern <- paste0("*.", file.type)



  # create new directory if it doesn't exist
  if (!dir.exists(new.dir)) {
    dir.create(new.dir)
  }


  # check to make sure both directories exist
  if (!dir.exists(old.dir)) {
    PEcAn.logger::logger.error("Old File directory does not exist. Please enter valid file path")
    error <- 1
  }

  if (!dir.exists(new.dir)) {
    PEcAn.logger::logger.error("New File directory does not exist. Please enter valid file path")
    error <- 1
  }

  if (basename(new.dir) != basename(old.dir)) {
    PEcAn.logger::logger.error("Basenames of files do not match")
  }

  # list files in the old directory
  old.files <- list.files(path = old.dir, pattern = file.pattern)

  # check to make sure there are files
  if (length(old.files) == 0) {
    PEcAn.logger::logger.warn("No files found")
    error <- 1
  }

  # create full file path
  full.old.file <- file.path(old.dir, old.files)


  ### Get BETY information ###
  con <- db.open(
    params = list(
      driver = "Postgres",
      dbname   = "bety",
      host     = "psql-pecan.bu.edu",
      user     = "bety",
      password = "bety")
  )

  # get matching dbfiles from BETY
  dbfile.path <- dirname(full.old.file)
  dbfiles <- dplyr::tbl(con, "dbfiles") %>%
    dplyr::collect() %>%
    dplyr::filter(.data$file_name %in% basename(full.old.file)) %>%
    dplyr::filter(.data$file_path %in% dbfile.path)


  # if there are matching db files
  if (dim(dbfiles)[1] > 0) {

    # Check to make sure files line up
    if (dim(dbfiles)[1] != length(full.old.file)) {
      PEcAn.logger::logger.warn("Files to be moved don't match up with BETY files, only moving the files that match")

      # IF DB FILES AND FULL FILES DONT MATCH, remove those not in BETY - will take care of the rest below
      index <- which(basename(full.old.file) %in% dbfiles$file_name)
      index1 <- seq(1, length(full.old.file))
      check <- index1[-which(index1 %in% index)]
      full.old.file <- full.old.file[-check]

      # record the number of files that are being moved
      files.changed <- length(full.old.file)
    }

    # Check to make sure the files line up
    if (dim(dbfiles)[1] != length(full.old.file)) {
      PEcAn.logger::logger.error("Files to be moved don't match up with BETY files, canceling move")
      error <- 1
    }


    # Make sure the files line up
    dbfiles <- dbfiles[order(dbfiles$file_name), ]
    full.old.file <- sort(full.old.file)

    # Record number of files moved and changed in BETY
    files.indb <- dim(dbfiles)[1]

    # Move files and update BETY
    if (error == 0) {
      for (i in 1:length(full.old.file)) {
        fs::file_move(full.old.file[i], new.dir)
        db.query(paste0("UPDATE dbfiles SET file_path= '", new.dir, "' where id=", dbfiles$id[i]), con)
      } # end i loop
    } # end error if statement
  } # end dbfile loop


  # if there are files that are in the folder but not in BETY, we can either register them or not
  if (dim(dbfiles)[1] == 0 | files.changed > 0) {

    # Recheck what files are in the directory since others may have been moved above
    old.files <- list.files(path = old.dir, pattern = file.pattern)

    # Recreate full file path
    full.old.file <- file.path(old.dir, old.files)


    # Error check again to make sure there aren't any matching dbfiles
    dbfile.path <- dirname(full.old.file)
    dbfiles <- dplyr::tbl(con, "dbfiles") %>%
      dplyr::collect() %>%
      dplyr::filter(.data$file_name %in% basename(full.old.file)) %>%
      dplyr::filter(.data$file_path %in% dbfile.path)

    if (dim(dbfiles)[1] > 0) {
      PEcAn.logger::logger.error("There are still dbfiles matching these files! Canceling link or registration")
      error <- 1
    }


    if (error == 0 & register == TRUE) {

      # Record how many files are being registered to BETY
      files.reg <- length(full.old.file)

      for (i in 1:length(full.old.file)) {
        file_path <- dirname(full.old.file[i])
        file_name <- basename(full.old.file[i])

        if (file.type == "nc") {
          mimetype <- "application/x-netcdf"
          formatname <- "CF Meteorology application"
        }
        else if (file.type == "clim") {
          mimetype <- "text/csv"
          formatname <- "Sipnet.climna"
        }
        else {
          PEcAn.logger::logger.error("File Type is currently not supported")
        }


        dbfile.input.insert(
          in.path = file_path,
          in.prefix = file_name,
          siteid = siteid,
          startdate = NULL,
          enddate = NULL,
          mimetype = mimetype,
          formatname = formatname,
          parentid = NA,
          con = con,
          hostname = PEcAn.remote::fqdn(),
          allow.conflicting.dates = FALSE,
          ens = FALSE
        )
      } # end i loop
    } # end error loop
  } # end register == TRUE

  if (error == 0 & register == FALSE) {
    # Create file path for symbolic link
    full.new.file <- file.path(new.dir, old.files)

    # Record number of files that will have a symbolic link made
    files.sym <- length(full.new.file)

    # Line up files
    full.new.file <- sort(full.new.file)
    full.old.file <- sort(full.old.file)

    # Check to make sure the files are the same length
    if (length(full.new.file) != length(full.old.file)) {
      PEcAn.logger::logger.error("Files to be moved don't match up with BETY. Canceling Move")
      error <- 1
    }

    # Move file and create symbolic link if there are no errors

    if (error == 0) {
      for (i in 1:length(full.old.file)) {
        fs::file_move(full.old.file[i], new.dir)
        R.utils::createLink(link = full.old.file[i], target = full.new.file[i])
      } # end i loop
    } # end error loop
  } # end Register == FALSE


  if (error > 0) {
    PEcAn.logger::logger.error("There was an error, files were not moved or linked")
  }

  if (error == 0) {
    PEcAn.logger::logger.info(paste0(files.changed + files.indb, " files were moved and updated on BETY,  ", files.sym, " were moved and had a symbolic link created, and ", files.reg, " files were moved and then registered in BETY"))
  }
} # end dbfile.move()
