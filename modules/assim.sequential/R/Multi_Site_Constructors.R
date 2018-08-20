
Contruct.Pf <- function(site.ids, var.names, X) {
  
  nsite <- length(site.ids)
  nvariable <- length(var.names)
  # I will make a big cov matrixand then I will populate it when cov of each site
  pf.matrix <-matrix(0,(nsite*nvariable),(nsite*nvariable))
  
  
  for (site in site.ids){
    #let's find out where this cov (for the current site needs to go in the main cov matrix)
    pos.in.matrix <- which(attr(X,"Site") %in% site)
   #forach site let's get the Xs
    pf.matrix [pos.in.matrix, pos.in.matrix] <- cov( X [, pos.in.matrix] )
  }
  
  
  # This is where we estimate the cov between state variables of different sites
  #I put this into a sperate loop so we can have more control over it
  site.cov.orders <- expand.grid(site.ids,site.ids) %>% filter( Var1 != Var2)
  
  for (i in 1:nrow(site.cov.orders)){
    # first we need to find out where to put it in the big matrix
    rows.in.matrix <- which(attr(X,"Site") %in% site.cov.orders[i,1])
    cols.in.matrix <- which(attr(X,"Site") %in% site.cov.orders[i,2])
    #estimated between these two sites
    two.site.cov<- cov( X [, c(rows.in.matrix, cols.in.matrix)] )[(nvariable+1):(2*nvariable),1:nvariable]
    # this is something we can pplay around with - I'm setting the off diag to zero 
    #here is where we do the localoziation
    two.site.cov [which(lower.tri(two.site.cov, diag = FALSE),T)%>% rbind (which(upper.tri(two.site.cov,F),T))] <- 0
  
    #putting it back to the main matrix
    pf.matrix [rows.in.matrix, cols.in.matrix] <- two.site.cov
    
    }
  return(pf.matrix)

}

#
Construct.R<-function(){
  
  
  
  
}