#' download.ERA5
#'
#' @param outfolder
#' @param start_date
#' @param end_date
#' @param site_id
#' @param lat.in
#' @param lon.in
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
download.ERA5 <-
  function(outfolder,
           start_date,
           end_date,
           site_id,
           lat.in,
           lon.in,
           dbparms=NULL,
           overwrite = FALSE,
           ...) {
 
    #
    input.args <- list(...)

    tryCatch({
      bety <- dplyr::src_postgres(dbname   = dbparms$dbname, 
                                  host     = dbparms$host, 
                                  user     = dbparms$user, 
                                  password = dbparms$password)
      
      con <- bety$con
      },
      error = function(e) {
       PEcAn.logger::logger.severe(paste0("",e))
      }
    )

    db.file <- PEcAn.DB::dbfile.input.check(
      siteid=site_id %>% as.character(),
      startdate = start_date,
      enddate = end_date,
      parentid = NA,
      mimetype="application/x-netcdf",
      formatname="CF Meteorology",
      con,
      hostname = PEcAn.remote::fqdn(),
      exact.dates = TRUE,
      pattern = "ERA5",
      return.all=TRUE
    ) %>%
      as.data.frame()
    
    
    if (nrow(db.file) >0){
      list.of.files <-db.file
    }else{

      #-- extract the site 
      list.of.files <- met2cf.ERA5(
        lat = lat.in,
        long = lon.in,
        start_date = start_date,
        end_date = end_date,
        sitename = site_id %>% as.character(),
        outfolder = outfolder,
        overwrite = FALSE,
        verbose = TRUE,
        dbparms=dbparms
      )
      
    }
    
return(list.of.files)
    
    
  }
