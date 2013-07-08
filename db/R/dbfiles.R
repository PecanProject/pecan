#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##' Function to insert a file into the dbfiles table as an input
##'
##' This will write into the dbfiles, inputs, machines and formats the required 
##' data to store the file
##' @name dbfile.insert
##' @title Insert file into tables
##' @param filename the name of the file to be inserted, this should not include the path to the file
##' @param pathname the path where the file is stored
##' @param siteid the id of the site that this data is applicable to 
##' @param startdate the start date of the data stored in the file
##' @param enddate the end date of the data stored in the file
##' @param mimetype the mime-type of the file
##' @param formatname the name of the format to distinguish between simmilair mime-types
##' @param con database connection object
##' @param hostname the name of the host where the file is stored, this will default to the name of the current machine
##' @param params database connection information
##' @return id of the input that is inserted
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##'   dbfile.insert('trait.data.Rdata', pathname, settings$run$site$id, settings$run$start.date, settings$run$end.date, 'application/x-RData', 'traits', dbcon)
##' }
dbfile.insert <- function(filename, pathname, siteid, startdate, enddate, mimetype, formatname, con, hostname=Sys.info()[['nodename']]) {
  # find appropriate format
  formatid <- db.query(paste0("SELECT id FROM formats WHERE mime_type='", mimetype, "' AND name='", formatname, "'"), con)[['id']]
  if (is.null(formatid)) {
    # insert format
    db.query(paste0("INSERT INTO formats (mime_type, name, created_at, updated_at) VALUES ('", mimetype, "', '", formatname, "', NOW(), NOW())"), con)
    formatid <- db.query(paste0("SELECT id FROM formats WHERE mime_type='", mimetype, "' AND name='", formatname, "'"), con)[['id']]
  }

  # find appropriate host
  hostid <- db.query(paste0("SELECT id FROM machines WHERE hostname='", Sys.info()[['nodename']], "'"), con)[['id']]
  if (is.null(hostid)) {
    # insert host
    db.query(paste0("INSERT INTO machines (hostname, created_at, updated_at) VALUES ('", Sys.info()[['nodename']], "', NOW(), NOW())"), con)
    hostid <- db.query(paste0("SELECT id FROM machines WHERE hostname='", Sys.info()[['nodename']], "'"), con)[['id']]
  }
  
  # find appropriate input
  inputid <- db.query(paste0("SELECT id FROM inputs WHERE site_id=", settings$run$site$id, " AND format_id=", formatid, " AND start_date='", settings$run$start.date, "' AND end_date='", settings$run$end.date, "';"), con)[['id']]
  if (is.null(inputid)) {
    # insert input
    db.query(paste0("INSERT INTO inputs (site_id, format_id, created_at, updated_at, start_date, end_date) VALUES (",
                    settings$run$site$id, ", ", formatid, ", NOW(), NOW(), '", settings$run$start.date, "', '", settings$run$end.date, "')"), con)
    inputid <- db.query(paste0("SELECT id FROM inputs WHERE site_id=", settings$run$site$id, " AND format_id=", formatid, " AND start_date='", settings$run$start.date, "' AND end_date='", settings$run$end.date, "';"), con)[['id']]
  }

  db.query(paste0("INSERT INTO dbfiles (container_type, container_id, file_name, file_path, machine_id, created_at, updated_at) VALUES (",
                  "'Inputs', ", inputid, ", '", filename, "', '", pathname, "', ", hostid, ", NOW(), NOW())"), con)

  invisible(db.query(paste0("SELECT * FROM dbfiles WHERE container_type='Inputs' AND container_id=", inputid, " AND file_name='", filename, "' AND file_path='", pathname, "' AND machine_id=", hostid, " ORDER BY id"), con)[['id']])
}

##' Function to check to see if a file exists in the dbfiles table as an input
##'
##' This will check the dbfiles, inputs, machines and formats tables to see if the
##' file exists 
##' @name dbfile.check
##' @title Check for a file in the input/dbfiles tables
##' @param siteid the id of the site that this data is applicable to 
##' @param startdate the start date of the data stored in the file
##' @param enddate the end date of the data stored in the file
##' @param mimetype the mime-type of the file
##' @param formatname the name of the format to distinguish between simmilair mime-types
##' @param con database connection object
##' @param hostname the name of the host where the file is stored, this will default to the name of the current machine
##' @param params database connection information
##' @return data.frame with the id, filename and pathname of the input that is requested
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##'   dbfile.check(settings$run$site$id, settings$run$start.date, settings$run$end.date, 'application/x-RData', 'traits', dbcon)
##' }
dbfile.check <- function(siteid, startdate, enddate, mimetype, formatname, con, hostname=Sys.info()[['nodename']]) {
  # find appropriate format
  formatid <- db.query(paste0("SELECT id FROM formats WHERE mime_type='", mimetype, "' AND name='", formatname, "'"), con)[['id']]
  if (is.null(formatid)) {
    # insert format
    db.query(paste0("INSERT INTO formats (mime_type, name, created_at, updated_at) VALUES ('", mimetype, "', '", formatname, "', NOW(), NOW())"), con)
    formatid <- db.query(paste0("SELECT id FROM formats WHERE mime_type='", mimetype, "' AND name='", formatname, "'"), con)[['id']]
  }

  # find appropriate host
  hostid <- db.query(paste0("SELECT id FROM machines WHERE hostname='", Sys.info()[['nodename']], "'"), con)[['id']]
  if (is.null(hostid)) {
    # insert host
    db.query(paste0("INSERT INTO machines (hostname, created_at, updated_at) VALUES ('", Sys.info()[['nodename']], "', NOW(), NOW())"), con)
    hostid <- db.query(paste0("SELECT id FROM machines WHERE hostname='", Sys.info()[['nodename']], "'"), con)[['id']]
  }
  
  # find appropriate input
  inputid <- db.query(paste0("SELECT id FROM inputs WHERE site_id=", settings$run$site$id, " AND format_id=", formatid, " AND start_date='", settings$run$start.date, "' AND end_date='", settings$run$end.date, "';"), con)[['id']]
  if (is.null(inputid)) {
    # insert input
    db.query(paste0("INSERT INTO inputs (site_id, format_id, created_at, updated_at, start_date, end_date) VALUES (",
                    settings$run$site$id, ", ", formatid, ", NOW(), NOW(), '", settings$run$start.date, "', '", settings$run$end.date, "')"), con)
    inputid <- db.query(paste0("SELECT id FROM inputs WHERE site_id=", settings$run$site$id, " AND format_id=", formatid, " AND start_date='", settings$run$start.date, "' AND end_date='", settings$run$end.date, "';"), con)[['id']]
  }

  invisible(db.query(paste0("SELECT * FROM dbfiles WHERE container_type='Inputs' AND container_id=", inputid, " AND machine_id=", hostid, " ORDER BY id"), con))
}
