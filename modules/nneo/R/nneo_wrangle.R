################################################################################
#' @title Download and wrangle NEON's sensor-based data products

#' @author Josh Roberti \email{jaroberti87@@gmail.com}\cr
#' Dave Durden\cr
#' Robert Lee

#' @description Retrieve related, sensor-based datasets at a given NEON site for
#' a custom time period, merged per measurement level and/or variable
#'
#' @param site_code (character) a site code. Required.
#' @param time_start (character) YYYY-MM-DD start day to check for files.
#' Required if time_end=NULL
#' @param time_end (character) YYYY-MM-DD end day to check for files.
#' Required if time_start=NULL
#' @param time_agr (numeric) Aggregation period, given in minutes, of NEON data
#' product(s) to be downloaded
#' (e.g. `1` corresponds to a 1-minute data product). Required.
#' @param data_var (character) NEON data product(s) to be downloaded. The user
#' may enter a specific, controlled, NEON data product or they may enter a
#' generic term if wanting multiple, similar, NEON data products. For instance,
#' defining data_var as "Photosythetically Active Radiation (Quantum Line)"
#' would satisfy the former, and defining data_var as "Active Radiation" or
#' just "Radiation" would satisfy the latter. Required
#' @param package (character) Package type to return, basic or expanded.
#' 'Expanded' datasets are only available for the smallest time_agr of each
#' NEON data product. NOTE: 'expanded' datasets are much larger than 'basic'
#' and will take considerably more time to download. Required

#' @return Returns a tibble of relevant data products from all spatial
#' locations at a NEON site for the custom time period.  Data products are
#' displayed via 'productName.spatialLocation', e.g., 'difRadMean.000.060' is
#' mean, diffuse, shortwave radiation as measured on the 6th measurement level
#' of a NEON tower (000.060), while 'linePARMean.005.000' is mean,
#' photosynthetically Active Radiation (PAR) as measured at the 5th soil plot
#' (005.000) of a NEON site.  More information regarding spatial location and
#' identifiers can be found at the references below.
#'
#' @references
#' [NEON Data Portal](http://data.neonscience.org/home)\cr
#' [NEON Data Availability](http://data.neonscience.org/view-data-availability)

#' @keywords Ecology, environmental data, climate, data, data products,
#' National Ecological Observatory Network (NEON), NEON, atmosphere

#' @examples
#' \dontrun{
#' #download 30-minute, radiation data from NEON's Bartlett site for Summer 2016
#' nneo_wrangle(site_code="BART", time_start="2016-06-20",
#' time_end="2016-09-21", data_var="radiation")
#' #download 1-minute, dust data from NEON's Sterling (STER) site for 2017-03-04
#' nneo_wrangle(site_code="STER",time_start="2017-03-04",data_var="dust",
#' time_agr=60)
#' }

#' @seealso Currently none

#' @export
# changelog and author contributions / copyrights
#   Josh Roberti (2016-10-24)
#     original creation
#   Dave Durden (2016-11-21)
#     Applying standard style following Wiki and packaging
#   Josh Roberti (2016-12-09)
#     Amended code so full expanded data package is preserved.
#     Fixed regex for greping files from API
#     version updated to 0.0.2.
#   Josh Roberti (2017-01-09)
#     Updated code so it can be (better) used external of in-house wrapper
#   Robert Lee (2017-04-07)
#     Changing formatting to conform to rOpenSci
#   Josh Roberti (2017-04-20 thru 04-24)
#     Amended code to submit to rOpenSci
#   Josh Roberti (2017-05-02)
#     Syntax fixes; reorganized data filtering logic to remove NA columns
################################################################################
nneo_wrangle<-function(site_code="BART",time_start="2016-06-20",time_end=NULL,
                   data_var= "active radiation",time_agr=30,package="basic"){
    #check for NULL dates:
    if(is.null(time_start) & is.null(time_end)){stop("Please enter a start time
                                                     and/or end time")}
    if(is.null(time_start)){time_start<-time_end}
    if(is.null(time_end)){time_end<-time_start}
    #grab site metadata:
    site_code_info<-nneo::nneo_site(site_code)
    #get data product code(s) if valid:
    dp_index<-grep(tolower(gsub("\\(", "",data_var)),gsub("\\(", "",
                        tolower(site_code_info$dataProducts$dataProductTitle)))
    product_code<-site_code_info$dataProducts$dataProductCode[dp_index]
    #if empty:
    if(length(product_code)==0){
      stop(paste0("data product(s) not available for: ", site_code))}
    #create year_month variable for nneo_data and nneo_file:
    year_month<-c(substr(time_start,0,7),substr(time_end,0,7))
    #check if it's same month - won't need to gather multiple monthly files:
    if(length(unique(year_month))==1){year_month<-unique(year_month)}
    #use nneo_data to get available file(s) via user input:
    var_data<-unlist(lapply(year_month, function(y) lapply(product_code,
                     function(x) nneo::nneo_data(product_code = x,
                                           site_code = site_code,
                                           year_month = y,
                                           package=package))),recursive = FALSE)
    #combine data filenames into one df:
    files_package<-do.call("rbind",lapply(lapply(var_data, "[[", "data"),
                                            "[[", "files"))
    #get filenames that match user requested time_agr and sort:
    searh_terms<-paste0(time_agr,"min.csv|",time_agr,"_minutes.csv")
    files_time_agr<-sort(files_package$name[grep(searh_terms,
                                                 files_package$name)])
    #get the data using nneo_file:
    data_all<-lapply(year_month, function(y) lapply(unique(files_time_agr),
                                   function(x) nneo::nneo_file(
                                     product_code = substr(x,15,27),
                                     site_code = site_code,
                                     year_month = y,
                                     filename = x)))
    #name first level of list:
    names(data_all)<-year_month
    #rbind dataframes of similar data:
    data_length<-unique(unlist(lapply(data_all, function(x) length(x))))
    interim<-list()
    for(i in 1:data_length){
      interim[[i]]<-do.call("rbind",lapply(data_all, "[[", i))
      names(interim[[i]])<-c(names(interim[[i]])[1:2],
                          paste0(names(interim[[i]][3:length(interim[[i]])]),
                                 substr(unique(files_time_agr),34,41)[i]))
    }
    #merge NEON data then convert startDateTime and endDateTime to POSIX format:
    data_merge<-Reduce(function(x, y) merge(x, y, by=c("startDateTime",
                                                       "endDateTime")),interim)
    #final filter by date:
    date_filt_start<-min(grep(time_start,
                              as.Date(substr(data_merge$startDateTime,0,10))))
    date_filt_end<-max(grep(time_end,
                            as.Date(substr(data_merge$startDateTime,0,10))))
    data_filtered<-data_merge[date_filt_start:date_filt_end,]
    #remove columns with all NAs:
    data_final<-data_merge[, !apply(is.na(data_filtered), 2, all)]
    #convert to Tibble format and output:
    return(tibble::as_data_frame(data_final))
}

