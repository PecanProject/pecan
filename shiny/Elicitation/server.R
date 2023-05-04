lapply(c( "shiny",
          "ggplot"
          ),function(pkg){
            if (!(pkg %in% installed.packages()[,1])){
                install.packages(pkg)
              }
            library(pkg,character.only = TRUE,quietly = TRUE)
          }
       )
if(!("SHELF" %in% install.packages()[,1])) devtools::install_github('OakleyJ/SHELF')

library(SHELF)
library(PEcAn.DB)


# Define server logic
server <- shinyServer(function(input, output, session) {
  
  ## assumes that variable allPriors is defined and saved in prior.RData
  if(!file.exists("prior.RData")){
    allPriors <- list()
    save(allPriors,file="prior.RData")
  }
  load("prior.RData")
  
  ## eventually get users, variables, PFTs from database
  bety <- NULL #PEcAn.visualization::betyConnect()
  
  ## quantiles to be elicited, in order of elicitation
  std.prior <- data.frame(quantiles=c(0,1,0.975,0.025,0.25,0.75,0.5),parameters = rep(NA,7))
  prior <- std.prior
  ii <- 1  ## within CDF counter
  jj <- length(allPriors) + 1  ## OVERALL prior counter
  myfit <- NA
  
  updatePrior <- eventReactive(input$Next, {
    ## determine which parameter needs updating
    i <- ii
    msg = ""
    cdf = ""
    
    ## get value from UI
    newVal <- as.numeric(input$paramVal)
    
    ## check that values in increasing order, if so insert
    myPrior <- prior
    if(!is.na(newVal) && i <= nrow(prior)){ 
      myPrior$parameters[i] <- newVal      
      sortPrior <- myPrior[order(myPrior$quantiles),]
      print(sortPrior$parameters)
      if(is.unsorted(sortPrior$parameters,na.rm=TRUE)){
        print("UNSORTED")
        msg <- paste(msg,"ERROR: CDF must be ascending\n")
      } else {
        i <- i + 1 ## increment counter for message
        prior <- myPrior
      }
      
    } else if(i < 3){
      ## only upper and lower values can be NA
      prior$parameters[i] <- NA
      i <- i + 1
    } else {
      ## Save and Reset for next
      allPriors[[jj]] <<- list(user = input$user,
                              variable = input$var,
                              prior=prior,
                              cdf = input$paramVal,
                              fit = myfit
                              )
      print("SAVED")
      i = 1  ## reset counter
      prior <<- std.prior ## reset prior
      myfit <<- NA ## reset prior fit
    }
    
    ## prep instructions
    if(i <= nrow(prior)){
      msg = paste0(msg,"Enter parameter value for ",prior$quantiles[i]*100,"% quantile")
    } else {
      msg = "Choose CDF"
    }
    
    ii <<- i ## update global counter
    
    ## debug messages
    print(getwd())
    print(c(jj,ii))
    
    ## save and return current state
    save(prior,allPriors,file="prior.RData")
    list(prior=prior,msg=msg)
  })

  output$instructions <- renderText({
    updatePrior()$msg
    })
  
  output$table <- renderTable({
    prior <<- updatePrior()$prior  ## update the global scope
    sortPrior <- prior[order(prior$quantiles),]
    sortPrior})
    
  
   output$outputPlot <- renderPlot({
     myPrior <- updatePrior()$prior
     sortPrior <- myPrior[order(prior$quantiles),]
     ## Empirical CDF Plot
     with(data = sortPrior, plot(parameters,quantiles,main=paste(input$user),type='b',lwd=2))
     
     ## when all values elicited, add fit CDFs to plot
     if(ii > nrow(prior)){
       ## Fit alternative functions
       len = nrow(prior)
       lower = ifelse(is.na(sortPrior$parameters[1]),-Inf,sortPrior$parameters[1])
       upper = upper = ifelse(is.na(sortPrior$parameters[len]),Inf,sortPrior$parameters[len])
       myfit <<- with(data = sortPrior,
                     fitdist(vals=parameters[2:(len-1)],probs=quantiles[2:(len-1)],lower,upper)
                     )
       lwd = 5*min(myfit$ssq,na.rm = TRUE)/myfit$ssq
       
       ## Would like to extend fitdist to additional BETY supported distributions:
       ## chisq, exp, f, unif, weibull
         xval = seq(min(prior$parameters,na.rm = TRUE),max(prior$parameters,na.rm=TRUE),length=1000)
         if(!is.na(myfit$Normal[1])){
           lines(xval,pnorm(xval,myfit$Normal$mean,myfit$Normal$sd),col=2,lwd=lwd[1])
         }
         if(!is.na(myfit$Gamma[1])){
           lines(xval,pgamma(xval,myfit$Gamma$shape,myfit$Gamma$rate),col=3,lwd=lwd[3])
         }
         if(!is.na(myfit$Log.normal[1])){
           lines(xval,plnorm(xval,myfit$Log.normal$mean.log.X,myfit$Log.normal$sd.log.X),col=4,lwd=lwd[4])
         }
         if(!is.na(myfit$Beta[1])){
           lines(xval,pbeta(xval,myfit$Beta$shape1,myfit$Beta$shape2),col=5,lwd=lwd[5])
         }
         legend("bottomright",legend=c("Normal","Gamma","Log.normal","Beta"),col=2:5,lwd=2)

     }
     PEcAn.visualization::add_icon()
   })
})

# runApp(port=5658, launch.browser=FALSE)
