query.bety.traits <- function(spstr, traits, con = NULL){
  ## check which traits in traits (those for which priors exist)

  trt = c("SLA","c2n_leaf","q",     "q")   
  syn = c("LMA","leafN",   "FRC_RC","fine_root_biomass")
  
  ##    have trait data available for species in spstr

  if(is.null(con)){
    con <- query.bety.con(...)
  }
  if(is.list(con)){
    print("query.bety.traits")
    print("WEB QUERY OF DATABASE NOTE IMPLEMENTED")
    return(NULL)
  }
  
  trait.data <- list()
  traits <- gsub('Vm0','Vcmax',gsub('root_respiration_factor','root_respiration_rate',traits))
  query <- paste("select distinct variables.name from traits join variables on (traits.variable_id = variables.id) where specie_id in (", spstr,") and variable_id in (select id from variables where name in (", vecpaste(c(traits,syn)),"));", sep = "")
  ##*TODO
  ## Need to write query for:
  ## first check pft_species for grass or tree 
  ##      if grass, root and shoot to calculate q
  ##      if tree, fine root and leaf to calculate q
  query.result <- dbSendQuery(con, query)
  traits.in.bety <- fetch(query.result, n = -1)

  ## match synonyms
  mch = which(syn %in% traits.in.bety$name)
  traits.in.bety$name[match(syn[mch],traits.in.bety$name)] = trt[mch]
  traits.in.bety = unique(traits.in.bety$name)

  ## grab trait data
  trait.data <- lapply(traits.in.bety, function(x)  query.bety.trait.data(x, spstr, con=con))
  if(FALSE){ ## alt loop for debugging
    for(trait in traits.in.bety){
      trait.data[[trait]] = query.bety.trait.data(trait,spstr,con)
    }
  }
  names(trait.data) <- gsub('root_respiration_rate', 'root_respiration_factor', traits.in.bety)
  return(trait.data)
}


