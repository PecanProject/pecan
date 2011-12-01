##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param spstr string of species id's from BETYdb database
##' @param priors vector of parameters for which priors have been specified 
##' @param con 
##' @return
##' @seealso \code{\link{query.bety.traits.data}}
##' @examples
##' spstr <- query.bety.pft_species('ebifarm.c4crop')$spstr
##' trvec <- c('leafN', 'SLA')
##' trait.data <- query.bety.traits(spstr, trvec)

query.bety.traits <- function(spstr, priors, con = NULL){
  ##*TODO
  ## Need to write query for:
  ## first check pft_species for grass or tree 
  ##      if grass, root and shoot to calculate fineroot2leaf
  ##      if tree, fine root and leaf to calculate fineroot2leaf

  browser()
  ## grab trait data
  trait.data <- lapply(priors, 
      function(trait) query.bety.trait.data(trait, spstr, con=con))
  names(trait.data) <- priors
  trait.data <- trait.data[!is.na(trait.data)]
  return(trait.data)
}


