library(shiny)
  
shinyServer(function(input, output) {

  n = 365*4
  x = seq(as.Date("2006-01-01"),as.Date("2006-12-31"),length=n)
  y = cumsum(rnorm(n))
  
  dataInput <- reactive({
    if(input$smooth %in% c(0,1)){
      return(list(x=x,y=y))
    }else{
      bin = rep(1:((n/input$smooth)+1),length=n,each=input$smooth)
      return(list(
        x = tapply(x,bin,mean,na.rm=TRUE),
        y = tapply(y,bin,mean,na.rm=TRUE)
      ))
    }
  })
  
  output$timePlot <- renderPlot({
    dat = dataInput()
    plot(dat$x,dat$y,type='l',lwd=2,
         xlim=input$dates) 
  })
    
})