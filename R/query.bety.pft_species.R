query.bety.pft_species <- function(pft){
## select plant id's associated with pft
  con <- query.bety.con()
  query <- paste("select id, genus, species.species from species where id in ( select specie_id from pfts_species where pft_id = (select id from pfts where name = '",pft,"'));", sep = "")
  q    <- dbSendQuery(con, query)
  species <- NA
  species <- fetch ( q, n = -1 )
  return(species)
}
