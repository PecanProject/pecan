query.bety.traits <- function(spstr, priors, con = NULL){
  trait.synonym.dictionary = list(SLA = 'LMA',
    c2n_leaf = 'leafN',
    root_respiration_maintenance = 'root_respiration_total',
    fineroot2leaf = 'FRC_RC',
    fineroot2leaf = 'fine_root_biomass')
  sel <- names(trait.synonym.dictionary) %in% priors
  trait.synonyms <-  unlist(trait.synonym.dictionary[sel])

  if(is.null(con)){
    con <- query.bety.con(...)
  }
  if(is.list(con)){
    print("query.bety.traits")
    print("WEB QUERY OF DATABASE NOTE IMPLEMENTED")
    return(NULL)
  }
  
  query <- paste("select distinct variables.name from traits join variables on (traits.variable_id = variables.id) where specie_id in (", spstr,");", sep = "")
  query.result <- dbSendQuery(con, query)
  traits <- fetch(query.result, n = -1)$name
 
  traits <- unique(traits)
  traits <- traits[which(traits %in% append(trait.synonyms, priors))]

  ##*TODO
  ## Need to write query for:
  ## first check pft_species for grass or tree 
  ##      if grass, root and shoot to calculate fineroot2leaf
  ##      if tree, fine root and leaf to calculate fineroot2leaf

  ## grab trait data
  trait.data <- lapply(traits, function(trait) query.bety.trait.data(trait, spstr, con=con))
  names(trait.data) <- traits
  return(trait.data)
}


