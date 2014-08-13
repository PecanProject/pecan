##' @name sample.parameters
##' @title sample parameters
##' @author Michael Dietze \email{dietze@bu.edu}
##' 
##' @param ne         number of ensemble members
##' @param settings   PEcAn settings object
##' @param con        PEcAn database connection
##' 
##' @return data frame of sampled parameters from the posterior distribution
##' 
sample.parameters <- function(ne,settings,con){
  
  ## grab posteriors from database
  if(is.null(settings$assim.sequential$prior)){
    pft.id =  db.query(paste0("SELECT id from pfts where name = '",settings$pfts$pft$name,"'"),con)
    priors =  db.query(paste0("SELECT * from posteriors where pft_id = ",pft.id),con)
    ## by default, use the most recent posterior as the prior
    settings$assim.sequential$prior = priors$id[which.max(priors$updated_at)]
  }
  ## load prior
  prior.db = db.query(paste0("SELECT * from dbfiles where container_type = 'Posterior' and container_id = ",
                             settings$assim.sequential$prior),con)
  prior.db = prior.db[grep("post.distns.Rdata",prior.db$file_name),]
  load(file.path(prior.db$file_path,"post.distns.Rdata"))
  ## sample from priors
  nvar <- nrow(post.distns)
  prior = as.data.frame(matrix(numeric(),ne,nvar))
  for(i in 1:nvar){
    if(post.distns$distn[i] == 'exp'){
      prior[,i] <- eval(parse(text=paste0("rexp(",ne,",",post.distns$parama[i],")")))
    }else{
      prior[,i] <- eval(parse(text=paste0("r",post.distns$distn[i],"(",ne,",",post.distns$parama[i],",",post.distns$paramb[i],")")))
    }
  }
  colnames(prior) = rownames(post.distns)
  
  return(prior)
}