##' Function to check whether there are processed ic files already
##' @name ic_status_check
##' @title ic_status_check
##' @export
##' @author Istem Fer
ic_status_check <- function(getveg.id = NULL, putveg.id = NULL,
                            input, site_id, model, start_date, con){
  
  ## input files for this site
  db_records <- db.query(paste("SELECT * from inputs where site_id =", site_id), con)
  
  ## not sure how to filter these records for IC files, thoughts:
  ## 1) the dates don't have to be exact like met files
  ## 2) currently workflow writes files with source name on it
  db_records <- db_records[grep(input$source, db_records$name), ]
  ## 3) there can be raw files without site id on their names (IC files have the site id on their names)
  db_records <- db_records[grep(site_id, db_records$name), ]
  
  if(nrow(db_records) != 0){
    # first look for ready files
    # can I assume that we'll always have model name in the format name?
    formatids <- db.query(paste0("SELECT * from formats where name like '%", model, "%'"), con)
    
    # and the intersection of these two can only be ic files?
    ic_records <- db_records[db_records$format_id %in% formatids$id, ]
    
    if(nrow(ic_records) != 0){
      # for this site, there are ready ic files created from this source
      # would have been easier if we had output name in the format name
      # but that's not the case for ED: output = css, formatname = ED2.cohort

      for(i in seq_along(ic_records$id)){
        ic_file_name <- db.query(paste("SELECT * from dbfiles where container_id =", 
                                       ic_records$id[i]), con)[["file_name"]]
        if(grepl(input$output, ic_file_name)){
          ic_file_info <- ic_records[ic_records$id == ic_records$id[i],]
          # One final year check here maybe? +/- 5 yrs from start_date, need something more sophisticated probably
          # need to discuss
          # if(lubridate::year(ic_file_info$end_date) <= lubridate::year(start_date) + 5 &
          # lubridate::year(ic_file_info$start_date) >= lubridate::year(start_date) - 5){
          putveg.id <- ic_records$id[i]
          #}
        }
      }
      
     
    }else{
      # now look for intermediate files created from this source 
      # we knot that there is only one format for intermediate step ic files, 1000000031 (spp.info) 
      ic_records <- db_records[db_records$format_id == 1000000031, ]
      
      if(nrow(ic_records) != 0){
        # year check here, or will there be a unique file for site/source?
        # for this site, there are intermediate ic files created from this source
        # if(lubridate::year(ic_file$end_date) <= lubridate::year(start_date) + 5 &
        # lubridate::year(ic_file$start_date) >= lubridate::year(start_date) - 5){
        getveg.id <- ic_records$id
        #}
      }
        
    }
    
    
  }
  
  return(list(getveg.id = getveg.id, putveg.id = putveg.id))
}
