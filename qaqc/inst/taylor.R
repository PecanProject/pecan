library("plotrix")
data=read.csv(file="pecan/qaqc/inst/data.csv",header=TRUE)
taylor.diagram(data$obs,data$model)