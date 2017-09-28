


find_variables_without_formats<-function(con){
  
  psql_of_variables<-PEcAn.DB::db.query("select * from variables;", con=con)
  psql_of_formats_variables<-PEcAn.DB::db.query("select * from formats_variables;", con=con)
  formats_without_inputs<-dplyr::anti_join(psql_of_formats, psql_of_inputs, by="format_id")
  return(formats_without_inputs) 
  
}