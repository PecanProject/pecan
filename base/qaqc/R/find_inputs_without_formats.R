##' find_inputs_without_formats
##' @author Tempest McCabe
##' 
##' @param user_id  Optional parameter to search by user_id
##' @param created_after Optional parameter to search by creation date. Date must be in form 'YYYY-MM-DD'
##' @param updated_after Optional parameter to search all entried updated after a certain date. Date must be in form 'YYYY-MM-DD'
##' @param con connection the the bety database
##' 
##' 
##' @description This is a function that returns a dataframe with all of the input entries that have no assosiated format records.
##' This is very rare in the database.   
##'
##' For more information on how to use this function see the "Pre-release-database-cleanup" script in the 'vignettes' folder
##' or look at the README

find_inputs_without_formats<-function(con, user_id=NULL, created_after=NULL, updated_after=NULL){
  
    format_command<-paste("select * from formats;")
    
  if(is.null(user_id) && is.null(created_after) && is.null(updated_after)){
    
    input_command<-paste("select * from inputs;")
    
    
  }else if (is.null((user_id)) && !is.null(created_after) && !is.null(updated_after)){
    
    input_command<-paste("select * from inputs where created_at>'", created_after,"' and updated_at>'",updated_after, "';", sep="")
    
    
    
  }else if(is.null(user_id) && is.null(created_after) && !is.null(updated_after)){
    
    input_command<-paste("select * from inputs where updated_at>'", updated_after, "';", sep="")
    
    
  }else if(is.null(user_id) && !is.null(created_after) && is.null(updated_after)){
    input_command<-paste("select * from inputs where created_at>'", created_after, "';", sep="")
    
    
  }else if(!is.null(user_id) && is.null(created_after) && is.null(updated_after)){
    
    input_command<-paste("select * from inputs where user_id='" ,user_id,"';", sep="")
  
    
  }else if(!is.null(user_id) && !is.null(created_after) && is.null(updated_after)){
    
    input_command<-paste("select * from inputs where user_id='" ,user_id,"' and created_at>'",created_after,"';", sep="")
    
  }else if(!is.null(user_id) && is.null(created_after) && !is.null(updated_after)){
     
    input_command<-paste("select * from inputs where user_id='" ,user_id,"' and updated_at> '",updated_after,"';", sep="")
    
  }else{
    
    PEcAn.logger::logger.error("user_id and/or data_rage have been set incorrectly")
    
  }
  
  psql_of_inputs<-PEcAn.DB::db.query(input_command, con=con)
  psql_of_formats<-PEcAn.DB::db.query(format_command, con=con)
  colnames(psql_of_formats)[1]<-"format_id"
  inputs_without_formats<-dplyr::anti_join(psql_of_inputs, psql_of_formats, by="format_id")
  inputs_without_formats$table_name<-rep("inputs", length(inputs_without_formats[,1]))
  inputs_without_formats<-as.data.frame(inputs_without_formats)
  return(inputs_without_formats) 
}
