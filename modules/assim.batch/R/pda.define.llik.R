##' Define PDA Likelihood Functions
##'
##' @title Define PDA Likelihood Functions
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return List of likelihood functions, one for each dataset to be assimilated against.
##'
##' @author Ryan Kelly
##' @export
pda.define.llik.fn <- function(settings) {
  # Currently just returns a single likelihood, assuming the data are flux NEE.
  llik.fn <- list()
  for(i in 1:length(settings$assim.batch$inputs)) {
    # NEE + heteroskedastic Laplace likelihood
    if(settings$assim.batch$inputs[[i]]$likelihood == "Laplace") {
        llik.fn[[i]] <- function(NEEm, obs) {
          NEE.resid <- abs(NEEm - obs$NEEo)
          NEE.pos <- (NEEm >= 0)
          LL <- c(dexp(NEE.resid[NEE.pos], 1/(obs$b0 + obs$bp*NEEm[NEE.pos]), log=TRUE), 
                  dexp(NEE.resid[!NEE.pos],1/(obs$b0 + obs$bn*NEEm[!NEE.pos]),log=TRUE))
          return(list(LL=sum(LL,na.rm=TRUE), n=sum(!is.na(LL))))
        }
    } else {
      # Default to Normal(0,1)
        llik.fn[[i]] <- function(model.out, obs.data) {
          LL <- dnorm(model.out - obs.data$data, log=TRUE)
          return(list(LL=sum(LL,na.rm=TRUE), n=sum(!is.na(LL))))
        }
    }
  }
  
  return(llik.fn)
}