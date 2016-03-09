##' @name matchInventoryRings
##' @export
matchInventoryRings <- function(trees,rings,extractor="TreeCode",nyears=30,coredOnly=TRUE){

  ## build tree codes
  id.build = function(x){do.call(paste0("to.",extractor),x)}
  names(trees) = toupper(names(trees))
  tree.ID = id.build(list(SITE=trees$SITE,PLOT=trees$PLOT,SUB=trees$SUB,TAG=trees$TAG))

  ## build tree ring codes
  if(is.list(rings)){
    ring.file <- rep(names(rings),times=sapply(rings,ncol))
    rings <- combine.rwl(rings)
  }
  ring.ID <- names(rings)
  id.extract = function(x){do.call(paste0("from.",extractor),list(x=x))}
  ring.info <- id.extract(ring.ID)

  ## matching up data sets by tree
  mch = match(tree.ID,ring.ID)
  cored = apply(!is.na(trees[,grep("DATE_CORE_COLLECT",names(trees))]),1,any)
  unmatched = which(cored & is.na(mch))
  write.table(tree.ID[unmatched],file="unmatched.txt")
  mch[duplicated(mch)] <- NA  ## if there's multiple stems, match the first

  ## combine data into one table
  combined = cbind(trees,t(as.matrix(rings))[mch,-(nyears-1):0 + nrow(rings)])
  if(coredOnly==TRUE){
    combined = combined[!is.na(combined$"2000"),]
  }
  return(combined)
}