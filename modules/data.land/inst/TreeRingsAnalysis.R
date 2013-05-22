## Tree ring analysis
setwd("~/Desktop/")
tuscon.data = Read_Tuscon("~/Desktop/revised/")

plot.data = read.plot("B_2012_Adult_Field_Data.csv")
fd = fuse_plot_treering(plot.data, tuscon.data)



#diametergrow(fd$diameters,fd$increments,fd$survival)
#plot2AGB(unit.conv=0.02)

