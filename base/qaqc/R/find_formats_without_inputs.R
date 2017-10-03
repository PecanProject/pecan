##' find_formats_without_inputs
##' @author Tempest McCabe
##' 
##' @param user_id  Optional parameter to search by user_id
##' @param created_after Optional parameter to search by creation date. Date must be in form 'YYYY-MM-DD'
##' @param updated_after Optional parameter to search all entried updated after a certain date. Date must be in form 'YYYY-MM-DD'
##' @param con connection the the bety database
##' 
##' 
##' @description This is a fucntion that returns a dataframe with all of the format entries that have no assosiated input records.  
##'
##' For more information on how to use this function see the "Pre-release-database-cleanup" script in the 'vignettes' folder
##' or look at the README


find_formats_without_inputs <- function(con, user_id = NULL, created_after = NULL, updated_after = NULL){

  input_command <- paste("select * from inputs;")
  
  if(is.null(user_id) && is.null(created_after) && is.null(updated_after)){
    
    format_command<-paste("select * from formats;")
    
  }else if (is.null((user_id)) && !is.null(created_after) && !is.null(updated_after)){
    
    format_command<-paste("select * from formats where created_at>'", created_after,"' and updated_at>'",updated_after, "';", sep = "")
      
  }else if(is.null(user_id) && is.null(created_after) && !is.null(updated_after)){
    
    format_command<-paste("select * from formats where updated_at>'", updated_after, "';", sep = "")
    
  }else if(is.null(user_id) && !is.null(created_after) && is.null(updated_after)){
    
    format_command<-paste("select * from formats where created_at>'", created_after, "';", sep = "")
    
  }else if(!is.null(user_id) && is.null(created_after) && is.null(updated_after)){
    
    format_command<-paste("select * from formats where user_id = '" ,user_id,"';")
    
  }else if(!is.null(user_id) && !is.null(created_after) && is.null(updated_after)){
    
    format_command<-paste("select * from formats where user_id = '" ,user_id,"' and created_at>'",created_after,"';", sep="")
    
  }else if(!is.null(user_id) && is.null(created_after) && !is.null(updated_after)){
    
    format_command<-paste("select * from formats where user_id = '" ,user_id,"' and updated_at> '",updated_after,"';", sep = "")
    
  }else{
    
    PEcAn.logger::logger.error("user_id and/or data_rage have been set incorrectly")
    
  }
  
  psql_of_inputs<-PEcAn.DB::db.query(input_command, con=con)
  psql_of_formats<-PEcAn.DB::db.query(format_command, con=con)
  colnames(psql_of_formats)[1]<-"format_id"
  formats_without_inputs<-dplyr::anti_join(psql_of_formats, psql_of_inputs, by="format_id")
  colnames(formats_without_inputs)[1]<-"id"
  
  formats_without_inputs$table_name<-rep("formats", length.out= length(formats_without_inputs[,1]))
  formats_without_inputs<-as.data.frame(formats_without_inputs)
  return(formats_without_inputs) 
  
  
}
