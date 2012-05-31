##' Convert queried data to format required by JAGS meta-analysis model 
##'
##' 
##' @title jagify
##' @param result 
##' @return result transformed to meet requirements of PEcAn meta-analysis model
##' @author David LeBauer
#==================================================================================================#


#---------------- Define jagify function. ---------------------------------------------------------#
jagify <- function(result){
  ## rename name column from treatment table to trt_id
  names(result)[names(result)=='name'] <- 'trt_id'
  
  result <- transform.nas(result)
  result <- assign.treatments(result)
  
  ## calculate summary statistics from experimental replicates
  result <- summarize.result(result)
  
  ## assign a unique sequential integer to site and trt; for trt, all controls == 0
  result <- subset(transform(result,
                             stat = as.numeric(stat),
                             n    = as.numeric(n),
                             site_id = as.integer(factor(site_id, unique(site_id))),
                             trt_id = as.integer(factor(trt_id, unique(c('control', as.character(trt_id))))),
                             greenhouse = as.integer(factor(greenhouse, unique(greenhouse))),
                             mean = mean,
                             citation_id = citation_id), 
                   select = c('stat', 'n', 'site_id', 'trt_id', 'mean', 'citation_id', 'greenhouse')) 

#   if(length(result$stat[!is.na(result$stat) & result$stat <= 0.0]) > 0) {
#     citationswithbadstats <- unique(result$citation_id[which(result$stat <= 0.0)])
#     warning.message <- paste('there are implausible values of SE: SE <= 0 \n',
#                              'for', trait, 'result from citation',citationswithbadstats,'\n',
#                              'SE <=0 set to NA \n')
#     warning(warning.message)
#     print(result)
#     result$stat[result$stat <= 0.0] <- NA
#   }
}
#--------------------------------------------------------------------------------------------------#


####################################################################################################
### EOF.  End of R script file.      				
####################################################################################################