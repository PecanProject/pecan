##' Generic function to query BETYdb
##'
##' Given a connection and a query, will return a query as a data frame
##' @title Query BETY
##' @param query SQL query string
##' @param con database connection object
##' @param ... optional arguments for connecting to database (e.g. password, user name, database)
##' @return data frame with query results
##' @examples
##' query.bety('select count(id) from traits;')
query.bety <- function(query,con=NULL,...){
  if(is.null(con)){
    con <- query.bety.con(...)
  }
  q  <- dbSendQuery(con, query)
  data <- fetch(q, n=-1)
  return(data)
}
##' Creates database connection object.
##'
##' Also removes any existing connections. 
##' @title Query BETY connection
##' @param ... optional arguments for connecting to database (e.g. password, user name, database)
##' @return database connection object
##' @examples
##' con <- query.bety.con()
query.bety.con <- function (...) {
    lapply(dbListConnections(MySQL()), dbDisconnect)
    dvr <- dbDriver("MySQL")
    con <- dbConnect(dvr, group = "ebi_analysis", ...)
    return(con)
}

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
  print(species)
  spstr <- vecpaste(species$id)
  return(spstr)
}
##' Query priors associated with a string of traits and plant functional type
##'
##' @title Query Priors
##' @param pft String name of the PFT in the database
##' @param trstr string of traits to query priors for
##' @param con database connection, can be list of arguments for connecting to database
##' @param ... optional arguments for connecting to database (e.g. password, user name, database)
##' @return return priors for a given pft
##' @examples
##' query.bety.priors('ebifarm.pavi', c('SLA', 'Vcmax', 'leaf_width'))
query.bety.priors <- function(pft, trstr, con=NULL,...){
  if(is.null(con)){
    con <- query.bety.con(...)
  }
  if(is.list(con)){
    print("query.bety.priors")
    print("WEB QUERY OF DATABASE NOT IMPLEMENTED")
    return(NULL)
  }
    
  ## query 1: query the prior_id s assoc. with pft
  query1 <- paste("select pfts_priors.prior_id from pfts_priors where pfts_priors.pft_id in (select pfts.id from pfts where pfts.name in ('",pft,"'));", sep = "")
  q1    <- dbSendQuery(con, query1)
  prior.id <- fetch(q1, n = -1 )$prior_id
  pr.id.str <- vecpaste(prior.id)
  if(is.null(prior.id)){
    print("**** NO PRIORS FOUND ****")
    return(prior.id)
  }
  
  ## query 2: query the variable names assoc. with priors.
  query2 <- paste("select distinct variables.name, distn, parama, paramb, n from priors join variables on priors.variable_id = variables.id where priors.id in (",pr.id.str,") and variables.name in (",trstr,");", sep = "")
  
  q2 <- dbSendQuery(con, query2)
  priors <- fetch ( q2, n = -1 )
  priors$name[priors$name == 'SLA_m2_per_gC'] <- 'SLA'
  rownames(priors) <- priors$name
  priors <- priors[, which(colnames(priors)!='name')]
  return(priors)
}
