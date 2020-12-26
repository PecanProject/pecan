##' Given input_id, return formats table and table of variables and units
##'
##' @param input_id,format.id numeric. Defaults to format.id if both provided
##' @param bety database connection
##' @param var.ids optional vector of variable IDs. If provided, limits results to these variables
##' @export query.format.vars
##'
##' @author Betsy Cowdery, Ankur Desai, Istem Fer
##'
query.format.vars <- function(bety, input.id=NA, format.id=NA, var.ids=NA) {

  if ((is.null(input.id)||is.na(input.id)) & (is.null(format.id)||is.na(format.id))){
    PEcAn.logger::logger.error("Must specify input id or format id")
  }

  # get input info either form input.id or format.id, depending which is provided
  # defaults to format.id if both provided
  # also query site information (id/lat/lon) if an input.id

  site.id <- NULL
  site.lat <- NULL
  site.lon <- NULL
  site.time_zone <- NULL

  if (is.na(format.id)) {
    f <- PEcAn.DB::db.query(
        query = paste("SELECT * from formats as f join inputs as i on f.id = i.format_id where i.id = ", input.id),
        con = bety
      )
    site.id <- PEcAn.DB::db.query(query = paste("SELECT site_id from inputs where id =", input.id), con = bety)
    if (is.data.frame(site.id) && nrow(site.id)>0) {
      site.id <- site.id$site_id
      site.info <-
        PEcAn.DB::db.query(
          query = paste(
            "SELECT id, time_zone, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id =",
            site.id
          ),
          con = bety
        )
      site.lat <- site.info$lat
      site.lon <- site.info$lon
      site.time_zone <- site.info$time_zone
    }
  } else {
    f <- PEcAn.DB::db.query(query = paste("SELECT * from formats where id = ", format.id), con = bety)
  }

  mimetype <- PEcAn.DB::db.query(query = paste("SELECT * from  mimetypes where id = ", f$mimetype_id), con = bety)[["type_string"]]
  f$mimetype <- utils::tail(unlist(strsplit(mimetype, "/")),1)

  # get variable names and units of input data
  fv <- PEcAn.DB::db.query(
    query = paste(
      "SELECT variable_id,name,unit,storage_type,column_number from formats_variables where format_id = ", f$id
    ),
    con = bety
  )

  if(all(!is.na(var.ids))){
    # Need to subset the formats table
    fv <- fv %>% dplyr::filter(variable_id %in% !!var.ids | storage_type != "")
    if(dim(fv)[1] == 0){
      PEcAn.logger::logger.error("None of your requested variables are available")
    }

  }

  if (nrow(fv)>0) {
    colnames(fv) <- c("variable_id", "input_name", "input_units", "storage_type", "column_number")
    fv$variable_id <- as.numeric(fv$variable_id)
    n <- dim(fv)[1]

    # get bety names and units
    vars <- as.data.frame(matrix(NA, ncol=2, nrow=n))
    colnames(vars) <- c("bety_name", "bety_units")

    # fv and vars need to go together from now on,
    # otherwise when there are more than one of the same variable_id it confuses merge
    vars_bety <- cbind(fv, vars)
    for(i in 1:n){
      vars_bety[i, (ncol(vars_bety) - 1):ncol(vars_bety)] <-
        as.matrix(PEcAn.DB::db.query(
          query = paste("SELECT name, units from variables where id = ", fv$variable_id[i]),
          con = bety
        ))
    }

    # Fill in input names and units with bety names and units if they are missing

    ind1 <- fv$input_name == ""
    vars_bety$input_name[ind1] <- vars_bety$bety_name[ind1]
    ind2 <- fv$input_units == ""
    vars_bety$input_units[ind2] <- vars_bety$bety_units[ind2]

    # Fill in CF vars
    # This will ultimately be useful when looking at met variables where CF != Bety

    #Fill in MstMIP vars
    #All PEcAn output is in MstMIP variables

    vars_full <- bety2pecan(vars_bety)

    header <- as.numeric(f$header)
    skip <- ifelse(is.na(as.numeric(f$skip)),0,as.numeric(f$skip))

    # Right now I'm making the inappropriate assumption that storage type will be
    # empty unless it's a time variable.
    # This is because I haven't come up for a good way to test that a character string is a date format

    st <- vars_full$storage_type
    time.row <- which(nchar(st)>1 & substr(st, 1,1) == "%")
    if(length(time.row) == 0) time.row <- NULL

    # Final format list
    format <- list(file_name = f$name,
                   mimetype = f$mimetype,
                   vars = vars_full,
                   skip = skip,
                   header = header,
                   na.strings=c("-9999","-6999","9999","NA"), # This shouldn't be hardcoded in, but not specified in format table ?
                   time.row = time.row,
                   site = site.id,
                   lat = site.lat,
                   lon = site.lon,
                   time_zone = site.time_zone
    )

    # Check that all bety units are convertible. If not, throw a warning.
    for (i in 1:length(format$vars$bety_units)) {

      if (format$vars$storage_type[i] != "") { #units with storage type are a special case

        # This would be a good place to put a test for valid sotrage types. Currently not implemented.

      } else if (udunits2::ud.are.convertible(format$vars$input_units[i], format$vars$pecan_units[i]) == FALSE) {

        if (PEcAn.utils::misc.are.convertible(format$vars$input_units[i], format$vars$pecan_units[i]) == FALSE) {
          PEcAn.logger::logger.warn("Units not convertible for",format$vars$input_name[i], "with units of",format$vars$input_units[i], ".  Please make sure the varible has units that can be converted to", format$vars$pecan_units[i])
        }

      }
    }


  } else {
    format <- list(file_name = f$name,
                   mimetype = f$mimetype,
                   na.strings=c("-9999","-6999","9999"), # This shouldn't be hardcoded in, but not specified in format table ?
                   time.row = NULL,
                   site = site.id,
                   lat = site.lat,
                   lon = site.lon,
                   time_zone = site.time_zone
    )
  }
  if (length(unique(format$vars$pecan_name))!=length(format$vars$pecan_name)) {
    unique_cols <- match(unique(format$vars$pecan_name), format$vars$pecan_name)
    PEcAn.logger::logger.warn(
      "There are duplicate columns in format record", format$file_name,
      ". If format is not wide format, check column(s)", format$vars$pecan_name[-unique_cols]
    )
  }


  return(format)
}
################################################################################
##' Convert BETY variable names to MsTMIP and subsequently PEcAn standard names
##'
##' @param vars_bety data frame with variable names and units
##' @export 
##'
##' @author Betsy Cowdery

bety2pecan <- function(vars_bety){
  
  # This needs to be moved to lazy load 
  bety_mstmip <- utils::read.csv(system.file("bety_mstmip_lookup.csv", package= "PEcAn.DB"), 
                          header = T, stringsAsFactors = FALSE)
  
  vars_full <- merge(vars_bety, bety_mstmip, by = "bety_name", all.x = TRUE)
  
  vars_full$pecan_name <- vars_full$mstmip_name
  vars_full$pecan_units <- vars_full$mstmip_units
  ind <- is.na(vars_full$pecan_name)
  vars_full$pecan_name[ind] <- vars_full$bety_name[ind]
  vars_full$pecan_units[ind] <- vars_full$bety_units[ind]
  
  dups <- unique(vars_full$pecan_name[duplicated(vars_full$pecan_name)])
  
  if("NEE" %in% dups){
    # This is a hack specific to Ameriflux!
    # It ultimately needs to be generalized, perhaps in a better version of 
    # bety2pecan that doesn't use a lookup table
    # In Ameriflux FC and NEE can map to NEE in mstmip/pecan standard
    # Thus if both are reported in the data, both will be converted to NEE
    # which creates a conflict. 
    # Here we go back to the bety name to determine which of those is NEE
    # The variable that is not NEE in bety (assuming it's FC) is discarded.
    
    keep <- which(vars_full$bety_name[which(vars_full$pecan_name == "NEE")] == "NEE")
    if(length(keep) == 1){
      discard <- vars_full$bety_name[which(vars_full$pecan_name == "NEE")][-keep]
      vars_full <- vars_full[!(vars_full$bety_name %in% discard),]
      dups <- unique(vars_full$pecan_name[duplicated(vars_full$pecan_name)])
    }
  }
  if(length(dups) > 0){
    PEcAn.logger::logger.warn(paste("The variable(s)", paste(dups, collapse = ", "),"are duplicated.
                             Currently we cannot support data with duplicate column names."))
  }
  return(vars_full)
}
