query.bety.traits <- function(spstr, priors, con = NULL){
  synonyms = list('LMA'='SLA',
                  'leafN'='c2n_leaf',
                  'FRC_RC'='q',
                  'fine_root_biomass'='q',
                  'root_respiration_factor'='root_respiration_rate',
                  'Vm0'='Vcmax')

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
  for(synonym in names(synonyms)) traits <- gsub(synonym, synonyms[synonym], traits)
  traits <- unique(traits)
  traits <- traits[which(traits %in% priors)]

  ##*TODO
  ## Need to write query for:
  ## first check pft_species for grass or tree 
  ##      if grass, root and shoot to calculate q
  ##      if tree, fine root and leaf to calculate q

  ## grab trait data
  trait.data <- lapply(traits, function(trait) query.bety.trait.data(trait, spstr, con=con))
  names(trait.data) <- traits
  return(trait.data)
}


