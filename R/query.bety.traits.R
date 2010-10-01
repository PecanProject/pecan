query.bety.traits <- function(spstr, trvec){
  ## check which traits in trvec (those for which priors exist)
  ##    have trait data available for species in spstr
  con <- query.bety.con()
  trait.data <- list()
  query <- paste("select distinct variables.name from traits join variables on (traits.variable_id = variables.id) where specie_id in (", spstr,") and variable_id in (select id from variables where name in (", vecpaste(trvec),"));", sep = "")
  ##*TODO
  ## Need to write query for:
  ## first check pft_species for grass or tree 
  ##      if grass, root and shoot to calculate q
  ##      if tree, fine root and leaf to calculate q
  query.result <- dbSendQuery(con, query)
  traits.in.bety <- fetch(query.result, n = -1)
  trait.data <- lapply(traits.in.bety$name, query.bety.trait.data, spstr)
  names(trait.data) <- traits.in.bety$name
  return(trait.data)
}


