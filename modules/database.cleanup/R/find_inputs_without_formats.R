find_inputs_without_formats<-function(con, user=NULL, created_after=NULL, updated_after=NULL){
  
  if(is.null(user)&&is.null(date_range)&& is.null(updated_after)){
    psql_of_inputs<-PEcAn.DB::db.query("select * from inputs;", con=con)
    psql_of_formats<-PEcAn.DB::db.query("select * from formats;", con=con)
    colnames(psql_of_formats)[1]<-"format_id"
    inputs_without_formats<-dplyr::anti_join(psql_of_inputs, psql_of_formats, by="format_id")
    return(inputs_without_formats) 
  }else if (is.null((user))&&!is.null(date_range) &&!is.null(updated_after)){
    psql_of_inputs<-PEcAn.DB::db.query("select * from inputs where;", con=con)
    psql_of_formats<-PEcAn.DB::db.query("select * from formats;", con=con)
    colnames(psql_of_formats)[1]<-"format_id"
    inputs_without_formats<-dplyr::anti_join(psql_of_inputs, psql_of_formats, by="format_id")
    return(inputs_without_formats) 
    
  }else if(!is.null(user)&&is.null(date_range)){
    
    
  }else if(!is.null(user)&&!is.null(date_range)){
  }else{
    PEcAn.logger::logger.error("User and/or data_rage have been set incorrectly")
  }
  
}