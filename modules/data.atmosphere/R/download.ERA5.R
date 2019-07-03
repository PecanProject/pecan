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
      as.data.frame()%>% 
      dplyr::filter(grepl("ERA5",file_name))
    
    
    if (nrow(db.file) >0){
      list.of.files <-db.file
    }else{
    
      #--- If there is no data for this site, then lets find the big raw tile.
      raw.tiles <- PEcAn.DB::dbfile.input.check(
        siteid = "1000026755",
        startdate = start_date %>% as.Date(),
        enddate = end_date %>% as.Date(),
        parentid = NA,
        mimetype = "application/x-netcdf",
        formatname = "CF Meteorology",
        con,
        hostname = PEcAn.remote::fqdn(),
        exact.dates = FALSE,
        pattern = "ERA5",
        return.all=TRUE
      ) %>%
        as.data.frame() %>%
        dplyr::filter(file_name == "ERA5")
      
      if (nrow(raw.tiles) > 0 ){
        data.folder <- dirname(raw.tiles$file_path)
      }else{
        PEcAn.logger::logger.severe("The raw ERA5 tiles needs to be downloaded and registered in the BETY under `USA`` site with 1000026755 id.")
      }
      

      #-- extract the site 
      list.of.files <- met2cf.ERA5(
        lat = lat.in,
        long = lon.in,
        start_date = start_date,
        end_date = end_date,
        sitename = site_id %>% as.character(),
        data.folder = data.folder,
        outfolder = outfolder,
        overwrite = FALSE,
        verbose = TRUE
      )
      
    }
    
return(list.of.files)
    
    
  }
