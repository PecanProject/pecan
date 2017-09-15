find_formats_without_inputs<-function(con){
  psql_of_inputs<-PEcAn.DB::db.query("select * from inputs;", con=con)
  psql_of_formats<-PEcAn.DB::db.query("select * from formats;", con=con)
  colnames(psql_of_formats)[1]<-"format_id"
  formats_without_inputs<-dplyr::anti_join(psql_of_formats, psql_of_inputs, by="format_id")
  return(formats_without_inputs) 
}