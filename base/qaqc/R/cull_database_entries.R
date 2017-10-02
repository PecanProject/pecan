##' cull_database_entries
##' @author Tempest McCabe
##' 
##' @param outdir  Directory from which the file will be read, and where the delete_log_FILE_NAME will be read to 
##' @param file_name The name of the file being read in
##' @param con connection the the bety database
##' @param machine_id Optional id of the machine that contains the bety entries.
##' 
##' @description This is a fucntion that takes in a table of records and deletes everything in the file. Please do not run this function without 
##' 1) Backing Up Bety 
##' 2) Checking the the file only contains entries to be deleted. 
##'
##' For more information on how to use this function see the "Pre-release-database-cleanup" script in the 'vignettes' folder
##' or look at the README
##' 
##' 

cull_database_entries<-function(table, outdir, file_name, con, machine_id = NULL){
  
  file<-paste(outdir,"/",file_name, sep = "")
  
  table<-read.table(file=file, header=TRUE, sep = "|")
  
  if( !"table_name" %in% names(table)){
    
    PEcAn.logger::logger.severe("Input file needs a 'table_name' column. Please check the file and the function that generated it.")
    
  }
  
  if("dbfile" %in% table$table_name){
    
    table<-table[table$machine_id==machine_id] #prevents deletion of files form other databases
    
  }
  
  log<-list()
  for(i in seq_along(table)){
    table_name<-as.character(table$table_name[i])
    id<-table$id[i]
    
    select_command<-paste("select * from ", table_name, " where id=", id, ";", sep="")
    log[i]<-PEcAn.DB::db.query(query=select_command, con=con)
    
    delete_command<-paste("DELETE from ", table_name, " where id=", id, ";", sep="")
    PEcAn.DB::db.query(delete_command, con=con)
    write.table(log, file=paste(outdir,"/deletion_log_of_",file_name, sep=""), row.names = FALSE)
  }
  
}
