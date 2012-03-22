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
query.bety.con <- function(...){
  lapply(dbListConnections(MySQL()), dbDisconnect) #first kill all connections
  dvr <- dbDriver ("MySQL")
  
  con <- dbConnect(dvr, group  = 'ebi_analysis',...)
  
  #KLUDGE: not all invocations of query.bet.con() make use of the settings file.
  #This effectively limits PEcAn to using ebi_analysis at certain places.
  #What follows is a quick fix - it relies on settings as a global variable,
  #which are generally recommended against
  if(exists('settings')){
    con <- dbConnect(dvr, group  = settings$database$name,
        dbname=settings$database$name, 
        password=settings$database$passwd, 
        username=settings$database$userid, 
        host=settings$database$host)
    return(con)
  }
  
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
query.bety.priors <- function(pft, trstr, out=NULL,con=NULL,...){
  if(is.null(con)){
    con <- query.bety.con(...)
  }
  if(is.list(con)){
    print("query.bety.priors")
    print("WEB QUERY OF DATABASE NOT IMPLEMENTED")
    return(NULL)
  }
  
  query.text <- paste("select variables.name, distn, parama, paramb, n",
      "from priors",
      "join variables on priors.variable_id = variables.id",
      "join pfts_priors on pfts_priors.prior_id = priors.id",
      "join pfts on pfts.id = pfts_priors.pft_id",
      "where pfts.name in (", vecpaste(pft), ")",
      "and variables.name in (", trstr, ");")
  query    <- dbSendQuery(con, query.text)
  priors <- fetch ( query, n = -1 )
  
  #HACK: this rename does not belong here. 
  #There should be a separate function for this called after query.bety.priors. 
  #-carl
  priors$name[priors$name == 'SLA_m2_per_gC'] <- 'SLA'
  
  if(nrow(priors) <= 0){
    warning(paste("No priors found for pft(s): ", pft))
    priors <- priors[, which(colnames(priors)!='name')]
    return(priors)
  }
  else {    
    rownames(priors) <- priors$name
    priors <- priors[, which(colnames(priors)!='name')]
    return(priors)
  }
}

##' Kill existing database connections
##'
##' resolves (provides workaround to) bug #769 caused by having too many open connections \code{Error during wrapup: RS-DBI driver: (cannot allocate a new connection -- maximum of 16 connections already opened)}
##' @title Kill existing database connections 
##' @return nothing, as a side effect closes all open connections
##' @author David LeBauer
killdbcons <- function(){
  for (i in dbListConnections(MySQL())) dbDisconnect(i)
}
