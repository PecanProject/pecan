##' @export find_formats_without_inputs
##' @author Tempest McCabe
##' 
##' @param user_id  Optional parameter to search by user_id
##' @param created_after Optional parameter to search by creation date. Date must be in form 'YYYY-MM-DD'.
##' @param created_before Optional parameter to search by creation date. Can be used in conjunciton with created_after to specify a spesific window. Date must be in form 'YYYY-MM-DD'.
##' @param updated_after Optional parameter to search all entried updated after a certain date. Date must be in form 'YYYY-MM-DD'.
##' @param updated_before Optional parameter to search all entried updated before a certain date. Date must be in form 'YYYY-MM-DD'.
##' @param con connection the the bety database
##' 
##' 
##' @description This is a fucntion that returns a dataframe with all of the format entries that have no assosiated input records.  
##'
##' For more information on how to use this function see the "Pre-release-database-cleanup" script in the 'vignettes' folder
##' or look at the README


find_formats_without_inputs <- function(con, user_id_code = NULL, created_after = NULL, updated_after = NULL, created_before = NULL, updated_before = NULL){

  input_command<-dplyr::tbl(con, 'inputs')
  
  format_command<-dplyr::tbl(con, 'formats')
  
  if(!is.null(user_id_code)){
    format_command<-dplyr::filter(format_command, user_id == user_id_code)
  }
  if(!is.null(created_before)){
    format_command<-dplyr::filter(format_command,created_at < created_before)  
  }
  if(!is.null(created_after)){
    format_command<-dplyr::filter(format_command,created_at > created_after)
  }
  if(!is.null(updated_before)){
    format_command<-dplyr::filter(format_command, updated_at < updated_before)
  }
  if(!is.null(updated_after)){
    format_command<-dplyr::filter(format_command, updated_at > updated_after)
  }
  
  format_command<-as.data.frame(format_command)
  input_command<-as.data.frame(input_command)
  
  colnames(format_command)[1]<-"format_id"
  formats_without_inputs<-dplyr::anti_join(format_command, input_command, by = "format_id")
  colnames(formats_without_inputs)[1]<-"id"
  
  formats_without_inputs$table_name<-rep("formats", length.out= length(formats_without_inputs[,1]))
  
  return(formats_without_inputs) 
}
