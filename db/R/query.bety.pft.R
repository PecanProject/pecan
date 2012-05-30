##'  select plant id's associated with pft
##'
##' @title Query species given pft name 
##' @param pft string pft name
##' @param con database connection
##' @param ... optional arguments for connecting to database (e.g. password, user name, database)
##' @return string of species.id for species associated with pft
##' @examples
##' query.bety.pft_species('ebifarm.pavi')
query.bety.pft_species <- function(pft,con=NULL,...){
  if(is.null(con)){
    con <- query.bety.con(...)
  }
  query <- paste("select id, genus, species.species from species where id in ( select specie_id from pfts_species where pft_id = (select id from pfts where name = '",pft,"'));", sep = "")
  q    <- dbSendQuery(con, query)
  species <- NA
  species <- fetch ( q, n = -1 )
  print(paste("List of species included in PFT: ",pft,sep=""))
  print(species)
  spstr <- vecpaste(species$id)
  return(spstr)
}
