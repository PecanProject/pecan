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
    browser()
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
      exact.dates = FALSE,
      pattern = NULL
    ) %>%
      as.data.frame()%>% 
      dplyr::filter(file_name=="ERA5")
    
    
    if (nrow>0){
      
    }else{
      met2cf.ERA5 (
        lat=lat.in,
        long=lon.in,
        years,
        sitename,
        data.folder,
        outfolder,
        overwrite = FALSE,
        verbose = TRUE
      )
    }
    

    
    
  }
