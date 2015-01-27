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
##' @name dbfile.input.insert
##' @title Insert file into tables
##' @param filename the name of the file to be inserted
##' @param siteid the id of the site that this data is applicable to 
##' @param startdate the start date of the data stored in the file
##' @param enddate the end date of the data stored in the file
##' @param mimetype the mime-type of the file
##' @param formatname the name of the format to distinguish between simmilair mime-types
##' @param parent the id of the parent of the input
##' @param con database connection object
##' @param hostname the name of the host where the file is stored, this will default to the name of the current machine
##' @param params database connection information
##' @return data.frame with the id, filename and pathname of the input that is requested
##' @export
##' @author Rob Kooper, Betsy Cowdery
##' @examples
##' \dontrun{
##'   dbfile.input.insert('trait.data.Rdata', siteid, startdate, enddate, 'application/x-RData', 'traits', dbcon)
##' }

dbfile.input.insert <- function(in.path, in.prefix, siteid, startdate, enddate, mimetype, formatname, parentid=NA, con, hostname=fqdn()) {
  
  name <- basename(in.path)
  filename <- file.path(in.path,in.prefix)
  
  if (hostname == "localhost") hostname <- fqdn();
  
  # find appropriate format
  formatid <- db.query(paste0("SELECT id FROM formats WHERE mime_type='", mimetype, "' AND name='", formatname, "'"), con)[['id']]
  if (is.null(formatid)) {
    # insert format
    db.query(paste0("INSERT INTO formats (mime_type, name, created_at, updated_at) VALUES ('", mimetype, "', '", formatname, "', NOW(), NOW())"), con)
    formatid <- db.query(paste0("SELECT id FROM formats WHERE mime_type='", mimetype, "' AND name='", formatname, "'"), con)[['id']]
  }
  
  # setup parent part of query if specified
  if (is.na(parentid)) {
    parent <- ""
  } else {
    parent <- paste0(" AND parent_id=", parentid)
  }
  
  # find appropriate input, if not in database, instert new input
  
  inputid <- db.query(paste0("SELECT id FROM inputs WHERE site_id=", siteid, " AND format_id=", formatid, " AND start_date='", startdate, "' AND end_date='", enddate, "'" , parent, ";"), con)[['id']]
  if (is.null(inputid)) {
    # insert input
    if(parent == ""){
      cmd <- paste0("INSERT INTO inputs (site_id, format_id, created_at, updated_at, start_date, end_date, name) VALUES (",
                    siteid, ", ", formatid, ", NOW(), NOW(), '", startdate, "', '", enddate,"','", name, "')")
    }else{
      cmd <- paste0("INSERT INTO inputs (site_id, format_id, created_at, updated_at, start_date, end_date, name, parent_id) VALUES (",
                    siteid, ", ", formatid, ", NOW(), NOW(), '", startdate, "', '", enddate,"','", name, "',",parentid,")")
    }
    db.query(cmd, con)
    # return input id
    inputid <- db.query(paste0("SELECT id FROM inputs WHERE site_id=", siteid, " AND format_id=", formatid, " AND start_date='", startdate, "' AND end_date='", enddate, "'" , parent, ";"), con)[['id']]
  }
  
  # find appropriate dbfile, if not in database, insert new dbfile 
  
  dbfileid <- dbfile.check('Input', inputid, con, hostname=fqdn()) 
  if(is.null(dbfileid)){
    #insert dbfile & return dbfile id
    dbfileid <- dbfile.insert(in.path, in.prefix, 'Input', inputid, con, hostname)
  }
  
  invisible(list(input.id = inputid, dfbile.id = dbfileid))
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
##' @param parent the id of the parent of the input
##' @param con database connection object
##' @param hostname the name of the host where the file is stored, this will default to the name of the current machine
##' @param params database connection information
##' @return data.frame with the id, filename and pathname of the input that is requested
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##'   dbfile.input.check(siteid, startdate, enddate, 'application/x-RData', 'traits', dbcon)
##' }
dbfile.input.check <- function(siteid, startdate, enddate, mimetype, formatname, parentid=NA, con, hostname=fqdn()) {
  if (hostname == "localhost") hostname <- fqdn();
  
  # find appropriate format
  formatid <- db.query(paste0("SELECT id FROM formats WHERE mime_type='", mimetype, "' AND name='", formatname, "'"), con)[['id']]
  if (is.null(formatid)) {
    return(invisible(data.frame()))
  }
  
  # setup parent part of query if specified
  if (is.na(parentid)) {
    parent <- ""
  } else {
    parent <- paste0(" AND parent_id=", parentid)
  }
  
  # find appropriate input
  inputid <- db.query(paste0("SELECT id FROM inputs WHERE site_id=", siteid, " AND format_id=", formatid, " AND start_date>='", startdate, "' AND end_date<='", enddate, "'" , parent, ";"), con)[['id']]
  if (is.null(inputid)) {
    return(invisible(data.frame()))
  }
  
  invisible(dbfile.check('Input', inputid, con, hostname))
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
##' @param params database connection information
##' @return data.frame with the id, filename and pathname of the posterior that is requested
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##'   dbfile.posterior.insert('trait.data.Rdata', pft, 'application/x-RData', 'traits', dbcon)
##' }
dbfile.posterior.insert <- function(filename, pft, mimetype, formatname, con, hostname=fqdn()) {
  if (hostname == "localhost") hostname <- fqdn();
  
  # find appropriate pft
  pftid <- db.query(paste0("SELECT id FROM pfts WHERE name='", pft, "'"), con)[['id']]
  if (is.null(pftid)) {
    logger.severe("Could not find pft, could not store file", filename)
  }
  
  # find appropriate format
  formatid <- db.query(paste0("SELECT id FROM formats WHERE mime_type='", mimetype, "' AND name='", formatname, "'"), con)[['id']]
  if (is.null(formatid)) {
    # insert format
    db.query(paste0("INSERT INTO formats (mime_type, name, created_at, updated_at) VALUES ('", mimetype, "', '", formatname, "', NOW(), NOW())"), con)
    formatid <- db.query(paste0("SELECT id FROM formats WHERE mime_type='", mimetype, "' AND name='", formatname, "'"), con)[['id']]
  }
  
  # find appropriate posterior
  posteriorid <- db.query(paste0("SELECT id FROM posteriors WHERE pft_id=", pftid, " AND format_id=", formatid), con)[['id']]
  if (is.null(posteriorid)) {
    # insert input
    db.query(paste0("INSERT INTO posteriors (pft_id, format_id, created_at, updated_at) VALUES (", pftid, ", ", formatid, ", NOW(), NOW())"), con)
    posteriorid <- db.query(paste0("SELECT id FROM posteriors WHERE pft_id=", pftid, " AND format_id=", formatid), con)[['id']]
  }
  
  invisible(dbfile.insert(filename, 'Posterior', posteriorid, con, hostname))
}

##' Function to check to see if a file exists in the dbfiles table as an input
##'
##' This will check the dbfiles, inputs, machines and formats tables to see if the
##' file exists 
##' @name dbfile.check
##' @title Check for a file in the input/dbfiles tables
##' @param pft the name of the pft that this data is applicable to 
##' @param mimetype the mime-type of the file
##' @param formatname the name of the format to distinguish between simmilair mime-types
##' @param con database connection object
##' @param hostname the name of the host where the file is stored, this will default to the name of the current machine
##' @param params database connection information
##' @return data.frame with the id, filename and pathname of the posterior that is requested
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##'   dbfile.posterior.check(pft, 'application/x-RData', 'traits', dbcon)
##' }
dbfile.posterior.check <- function(pft, mimetype, formatname, con, hostname=fqdn()) {
  if (hostname == "localhost") hostname <- fqdn();
  
  # find appropriate pft
  pftid <- db.query(paste0("SELECT id FROM pfts WHERE name='", pft, "'"), con)[['id']]
  if (is.null(pftid)) {
    return(invisible(data.frame()))
  }
  
  # find appropriate format
  formatid <- db.query(paste0("SELECT id FROM formats WHERE mime_type='", mimetype, "' AND name='", formatname, "'"), con)[['id']]
  if (is.null(formatid)) {
    return(invisible(data.frame()))
  }
  
  # find appropriate posterior
  posteriorid <- db.query(paste0("SELECT id FROM posteriors WHERE pft_id=", pftid, " AND format_id=", formatid), con)[['id']]
  if (is.null(posteriorid)) {
    return(invisible(data.frame()))
  }
  
  invisible(dbfile.check('Posterior', posteriorid, con, hostname))
}

##' Function to insert a file into the dbfiles table
##'
##' This will write into the dbfiles and machines the required data to store the file
##' @name dbfile.insert
##' @title Insert file into tables
##' @param filename the name of the file to be inserted
##' @param con database connection object
##' @param hostname the name of the host where the file is stored, this will default to the name of the current machine
##' @param params database connection information
##' @return data.frame with the id, filename and pathname of the file that is written
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##'   dbfile.insert('somefile.txt', 'Input', 7, dbcon)
##' }
dbfile.insert <- function(in.path,in.prefix, type, id, con, hostname=fqdn()) {
  if (hostname == "localhost") hostname <- fqdn();
  
  # find appropriate host
  hostid <- db.query(paste0("SELECT id FROM machines WHERE hostname='", hostname, "'"), con)[['id']]
  if (is.null(hostid)) {
    # insert host
    db.query(paste0("INSERT INTO machines (hostname, created_at, updated_at) VALUES ('", hostname, "', NOW(), NOW())"), con)
    hostid <- db.query(paste0("SELECT id FROM machines WHERE hostname='", hostname, "'"), con)[['id']]
  }
  
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  db.query(paste0("INSERT INTO dbfiles (container_type, container_id, file_name, file_path, machine_id, created_at, updated_at) VALUES (",
                  "'", type, "', ", id, ", '", in.prefix, "', '", in.path, "', ", hostid, ", '", now, "', '", now, "')"), con)
  
  invisible(db.query(paste0("SELECT * FROM dbfiles WHERE container_type='", type, "' AND container_id=", id, " AND created_at='", now, "' ORDER BY id DESC LIMIT 1"), con)[['id']])
}

##' Function to check to see if a file exists in the dbfiles table
##'
##' This will check the dbfiles and machines to see if the file exists 
##' @name dbfile.check
##' @title Check for a file in the dbfiles tables
##' @param type the type of dbfile (Input, Posterior)
##' @param id the id of container type
##' @param con database connection object
##' @param hostname the name of the host where the file is stored, this will default to the name of the current machine
##' @return data.frame with the id, filename and pathname of all the files that are associated
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##'   dbfile.check('Input', 7, dbcon)
##' }
dbfile.check <- function(type, id, con, hostname=fqdn()) {
  if (hostname == "localhost") hostname <- fqdn()
  
  # find appropriate host
  hostid <- db.query(paste0("SELECT id FROM machines WHERE hostname='", hostname, "'"), con)[['id']]
  if (is.null(hostid)) {
    return(invisible(data.frame()))
  }
  
  invisible(db.query(paste0("SELECT * FROM dbfiles WHERE container_type='", type, "' AND container_id=", id, " AND machine_id=", hostid), con))  
}


##' Function to return full path to a file using dbfiles table
##'
##' This will check the dbfiles and machines to see if the file exists,
##' and return the full filename with path to the first one found. If 
##' none is found it will return NA.
##'
##' @name dbfile.file
##' @title Return file from the dbfiles tables
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
dbfile.file <- function(type, id, con, hostname=fqdn()) {
  if (hostname == "localhost") hostname <- fqdn();
  
  files <- dbfile.check(type, id, con, hostname)
  
  if(nrow(files) > 1) {
    logger.warn("multiple files found for", id, "returned; using the first one found")
    invisible(file.path(files[1, 'file_path'], files[1, 'file_name']))
  } else if (nrow(files) == 1) {
    invisible(file.path(files[1, 'file_path'], files[1, 'file_name']))
  } else {
    logger.warn("no files found for ", id, "in database")
    invisible(NA)
  }
}

##' Function to return id to containter type given a filename.
##'
##' This will check the dbfiles and machines to see if the file exists,
##' and return the id of the container type of the first one found. If 
##' none is found it will return NA.
##'
##' @name dbfile.file
##' @title Return id from the dbfiles tables
##' @param type the type of dbfile (Input, Posterior)
##' @param file the full pathname to the file
##' @param con database connection object
##' @param hostname the name of the host where the file is stored, this will default to the name of the current machine
##' @return filename on host, or NA if none found
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##'   dbfile.id('Model', '/usr/local/bin/sipnet', dbcon)
##' }
dbfile.id <- function(type, file, con, hostname=fqdn()) {
  if (hostname == "localhost") hostname <- fqdn();
  
  # find appropriate host
  hostid <- db.query(paste0("SELECT id FROM machines WHERE hostname='", hostname, "'"), con)[['id']]
  if (is.null(hostid)) {
    return(invisible(NA))
  }
  
  # find file
  file_name <- basename(file)
  file_path <- dirname(file)
  ids <- db.query(paste0("SELECT container_id FROM dbfiles WHERE container_type='", type, "' AND file_path='", file_path, "' AND file_name='", file_name, "' AND machine_id=", hostid), con)
  
  if(nrow(ids) > 1) {
    logger.warn("multiple ids found for", file, "returned; using the first one found")
    invisible(ids[1, 'container_id'])
  } else if (nrow(ids) == 1) {
    invisible(ids[1, 'container_id'])
  } else {
    logger.warn("no id found for", file, "in database")
    invisible(NA)
  }
}

