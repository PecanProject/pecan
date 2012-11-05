## FBBmodel setup

# load data
data = read.csv(datFile, header=T)

# select which leaf to analyze
dat = data[which(data$id == species.id),]
dat = dat[which(dat$leaf == leaf),]
npoints = nrow(dat)

# define data variables
An.obs 	= dat$Photo			# observed An
gs.obs 	= dat$Cond			# observed gs
Ca 		= dat$CO2S 			# atmospheric [C02]
H 		= dat$RH_S/100		# realtive humidity
Q 		= dat$PARi			# PAR
Ci 		= Ca-An.obs/gs.obs	# intracellular [CO2]

# determine proper sorting of measurements
sorted = sort.int(Ca,index.return=TRUE)
index = sorted$ix