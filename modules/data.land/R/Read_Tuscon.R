
Read_Tuscon <- function(folder){

  library(dplR)
  
  filenames <- dir(folder,pattern =  "TXT",full.names=TRUE)
  filenames <- c(filenames,dir(folder,pattern =  "rwl",full.names=TRUE))
  filedata <- list()
  for (file in filenames){
    filedata[[file]] <- read.tucson(file, header = FALSE)
  }

  return(filedata)


  library(plyr)
  library(stringr)
  library(ggplot2)

  lapply(filedata, nrow)
  names(filedata)

  gr <- function(x){
    x <- x[,1]
    mean(x[(length(x)-5):length(x)])
  }                                   ##average last 5yr growth
  growth <- ldply(filedata, gr)
  colnames(growth)[1] <- "name"
  growth <- transform(growth, 
                      Site = "V",
                      Plot = substr(name,2,2),
                      Subplot = substr(name,3,3),
                      Tag = str_sub(name, 5,-5))
  
  mydata <- merge(data2, growth, by = c("Plot", "Subplot", "Tag"))  ##merge growth, plot data
  colnames(mydata)[colnames(mydata) == "V1"] <- "rate"
  
  mydata <- transform(mydata,
                      subplot = Subplot,
                      spp = Spp,
                      dbh = DBH11,
                    rgr = rate/DBH11)
  attach(mydata)
  
  ##1st set of example plots from Nick & David
                                        #now you can start  plotting
  library(car)
  scatterplot.matrix(~rate+dbh+x+y|spp,data=mydata)
  boxplot(dbh~spp)
  plot(rate~dbh+spp)
  hist(mydata$DBH11)

  ## and  modeling
  summary(lm(rate~as.factor(spp)))
  summary(aov(rate~as.factor(spp) + as.factor(subplot)))
  
  
  panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
    {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r <- abs(cor(x, y))
      txt <- format(c(r, 0.123456789), digits=digits)[1]
      txt <- paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
      
      test <- cor.test(x,y)
                                        # borrowed from printCoefmat
      Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " "))
      
      text(0.5, 0.5, txt, cex = cex * r)
  text(.8, .8, Signif, cex=cex, col=2)
    }
  pairs(mydata[,c("dbh","rate","rgr","x","y","spp")], 
        lower.panel=panel.smooth, upper.panel=panel.cor)
  
}
