#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
####################################################################################################
#/file									  			   
#								          			   
#				ED2 Meteorology Driver Utilities				   
#				-- v1					 			   
#				-- Draft version.    		   
#									  			   
####################################################################################################

#---------------- Close all devices and delete all variables. -------------------------------------#
rm(list=ls(all=TRUE)) # clear workspace
graphics.off()	# close any open graphics
#--------------------------------------------------------------------------------------------------#


#---------------- Import command arguments  -------------------------------------------------------#
args = commandArgs(trailingOnly = TRUE) # import any needed arguments for the terminal
print(args) 				#---> Print command arguments in R output. For debugging.

######### Define model run location #########
if (args[1]=="pwd"){
	model_run=paste(getwd(),"/",sep="")	# if set as pwd
}else{
	model_run=paste(args[1],"/",sep="")	# if full path given
}
#--------------------------------------------------------------------------------------------------#


#---------------- Load required libraries ---------------------------------------------------------#
# Info: Loads required R libraries and warns if package is not availible.
ok = require(hdf5) ; if (! ok) stop("Package hdf5 is not available...")
ok = require(XML) ; if (! ok) stop("Package XML is not available...")
#--------------------------------------------------------------------------------------------------#


####################################################################################################
###################################### START SCRIPT ################################################
print('**************************************************')
print('**************** STARTING SCRIPT *****************')
print('**************************************************')
print(' ')


#---------------- Setup script output(s) ----------------------------------------------------------#
# Info: Generate output folder for diagnostic plots.  Will create folder if it doesn't already
# 	exist.
output_dir = paste(model_run,"MET_OUTPUT/",sep="")
if (! file.exists(output_dir)) dir.create(output_dir)
#--------------------------------------------------------------------------------------------------#


#---------------- Parse Info From ED2IN File ------------------------------------------------------#
# Info: Parse ED2IN txt file to determine location of ED_MET_DRIVER_HEADER_DB.  
#	Then parse ED_MET_DRIVER_HEADER for location of met driver hdf5 files.

# Location of ED_MET_DRIVER_HEADER
setwd(model_run)  # CWD

# ------ OLD METHOD ------
#system(paste("grep"," ","NL%ED_MET_DRIVER_DB"," ","*ED2IN*"," ","> loc"))
#ED_MET_DRIVER_DB = readLines("loc") 
# ------------------------

# PARSE ED_MET_DRIVER_DB LOCATION
if (args[2]=="-f"){
	ED_MET_DRIVER_DB = system(paste("grep"," ","NL%ED_MET_DRIVER_DB"," ","*ED2IN*"),intern=TRUE)
}else{
	ED_MET_DRIVER_DB = system(paste("grep"," ","NL%ED_MET_DRIVER_DB"," ",args[2]),intern=TRUE)
}

# Locate met header info in ED2IN file
indices = gregexpr("'", ED_MET_DRIVER_DB)[[1]]
met_header_loc = substr(ED_MET_DRIVER_DB,indices[1]+1, indices[2]-1)

# Location of met driver files
ED_MET_DRIVER=readLines(met_header_loc)
MET_DRIVERS=ED_MET_DRIVER[3]
indices = gregexpr("/", MET_DRIVERS)[[1]]
MET_DRIVER_LOC = substr(MET_DRIVERS,indices[1]+1, indices[length(indices)]-1)

# List of Met files
met_files = list.files(path=paste("/",MET_DRIVER_LOC,"/",sep=""),pattern=".h5")

# Extract years and months from filenames. Uses grep and regexp
setwd(paste("/",MET_DRIVER_LOC,"/",sep="")) # CWD

# Probably should change this so it is system independent.  E.g. R internal commands.
yrs = as.numeric(system(paste("ls *.h5 | ","grep"," ","-o"," ",'[1,2][0-9][0-9][0-9]',sep="")
,intern=TRUE))

first.yr  =min(unique(yrs)) 	# first year of met data
last.yr	  =max(unique(yrs)) 	# last year of met data
yr.range = (last.yr-first.yr)+1	# number of years

# Probably should change this so it is system independent.  E.g. R internal commands.
months = system(paste("ls *.h5 | ","grep"," ","-o -E"," ",
"'JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC'",sep=""),intern=TRUE)
#--------------------------------------------------------------------------------------------------#


#---------------- Generate Met Arrays from HDF Files ----------------------------------------------#
# Info: Create vectors of full time-series met data

setwd(output_dir) # Change to output directory
print(paste("----- Start year: ",first.yr))
print(paste("----- End year: ",last.yr))
print(paste("----- Number of years: ",yr.range))

# initialize output arrays
vgrd 		= numeric(0)		# meridional wind [m/s]. may be empty
vddsf		= numeric(0)		# visible diffuse downward solar radiation [W/m2]
vbdsf		= numeric(0)		# visible beam downward solar radiation [W/m2]
ugrd		= numeric(0)		# zonal wind [m/s]
Tair		= numeric(0)		# temperature [K]. hdf5 var name "tmp"
sh		= numeric(0)		# specific humidity [kg_H2O/kg_air]
pres		= numeric(0)		# pressure [Pa]
prate		= numeric(0)		# precipitation rate [kg_H2O/m2/s]
nddsf		= numeric(0)		# near IR diffuse downward solar radiation [W/m2]
nbdsf		= numeric(0)		# near IR beam downward solar radiation [W/m2]
hgt		= numeric(0)		# geopotential height [m]
dlwrf		= numeric(0)		# downward long wave radiation [W/m2]
CO2		= numeric(0)		# surface co2 concentration [ppm]

time		= 1
time_year	= numeric(0)
cnt		= 1
month_order <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

# Loops over met files
for (i in sort(unique(yrs))){

       	sel <- which(yrs == i) # which year?

	for(m in 1:12){
		msel <- sel[which(months[sel] == month_order[m])] # select appropriate month
		print(paste("**** PROCESSING: ",met_files[msel])) # output status to screen
		met = hdf5load(paste("/",MET_DRIVER_LOC,"/",met_files[msel],sep=""), 
		load = FALSE,tidy=TRUE)				  # load met file
		
		# put data into arrays
		if(cnt == 1){
			ugrd[1:length(as.numeric(met$ugrd))]=as.numeric(met$ugrd)
			vgrd[1:length(as.numeric(met$vgrd))]=as.numeric(met$vgrd)
			vddsf[1:length(as.numeric(met$vddsf))]=as.numeric(met$vddsf)
			vbdsf[1:length(as.numeric(met$vbdsf))]=as.numeric(met$vbdsf)
			Tair[1:length(as.numeric(met$tmp))]=as.numeric(met$tmp)
			sh[1:length(as.numeric(met$sh))]=as.numeric(met$sh)
			pres[1:length(as.numeric(met$pres))]=as.numeric(met$pres)
			prate[1:length(as.numeric(met$prate))]=as.numeric(met$prate)
			nddsf[1:length(as.numeric(met$nddsf))]=as.numeric(met$nddsf)
			nbdsf[1:length(as.numeric(met$nbdsf))]=as.numeric(met$nbdsf)
			hgt[1:length(as.numeric(met$hgt))]=as.numeric(met$hgt)
			dlwrf[1:length(as.numeric(met$dlwrf))]=as.numeric(met$dlwrf)
			CO2[1:length(as.numeric(met$co2))]=as.numeric(met$co2)
    		}else{
			# populate arrays
			start	= length(ugrd)
			ugrd	= append(ugrd,as.numeric(met$ugrd),after=start)

			rm(start);start=length(vgrd)
			vgrd	= append(vgrd,as.numeric(met$vgrd),after=start)

			rm(start);start=length(vddsf)
			vddsf	= append(vddsf,as.numeric(met$vddsf),after=start)

			rm(start);start=length(vbdsf)
			vbdsf	= append(vbdsf,as.numeric(met$vbdsf),after=start)

			rm(start);start=length(Tair)
			Tair 	= append(Tair,as.numeric(met$tmp),after=start)

			rm(start);start=length(sh)
			sh 	= append(sh,as.numeric(met$sh),after=start)

			rm(start);start=length(pres)
			pres 	= append(pres,as.numeric(met$pres),after=start)

			rm(start);start=length(prate)
			prate 	= append(prate,as.numeric(met$prate),after=start)

			rm(start);start=length(nddsf)
			nddsf 	= append(nddsf,as.numeric(met$nddsf),after=start)

			rm(start);start=length(nbdsf)
			nbdsf 	= append(nbdsf,as.numeric(met$nbdsf),after=start)

			rm(start);start=length(hgt)
			hgt 	= append(hgt,as.numeric(met$hgt),after=start)

			rm(start);start=length(dlwrf)
			dlwrf 	= append(dlwrf,as.numeric(met$dlwrf),after=start)

			rm(start);start=length(CO2)
			CO2 	= append(CO2,as.numeric(met$co2),after=start)
    		} 
		cnt <- cnt + 1
		rm(met)	# remove met data for last year
	}
	obs = length(Tair)
	time_year[time] = data.frame(length=as.numeric(obs))
	time=time+1
}

######### Calculate cumulative precip by year #######
cumprcp	= numeric(0)
for(j in 1:length(unique(yrs))){
	if(j == 1){
		cumprcp[1:as.numeric(time_year[j])]=cumsum(prate[1:as.numeric(time_year[j])])
	}else{
		start	= length(cumprcp)
		cumprcp[start:as.numeric(time_year[j])]=cumsum(prate[start:as.numeric(time_year[j])])
	}
}
#--------------------------------------------------------------------------------------------------#


#---------------- Output --------------------------------------------------------------------------#
# Info: Generate output diagnostics

if (args[3]==1) {
	print("************* OUTPUT STATS ONLY *************")
	print("************* !NOT COMPLETED YET! *************")
	#TODO: Create statistics here
}else{
	message('')
	print("************* OUTPUT STATS & DIAGNOSTIC PLOTS *************")
  message("************* OUTPUT STATS & DIAGNOSTIC PLOTS *************")
	#---------------- Generate Diagnostic Plots -----------------------------------------------#
	ptcex 	= 1.0	
	axcex 	= 1.4
	labcex 	= 1.8
	maincex = 1.5
	xaxis 	= 1:length(Tair)
	years 	= unique(yrs)
	years2 	= c(years[2:yr.range],years[yr.range]+1)

	# TODO: Put in a figure loop here rather than being
	# explicit with each variable.
	vars = list("vgrd","vddsf","vbdsf","ugrd","Tair","sh","pres","prate","cumprcp",
		"nddsf","nbdsf","hgt","dlwrf","CO2")
	################## Generate Plots #################
	
	############# vgrd
	num=1
  message('')
	message("************* Plotting meridional wind [m/s] *************")
	print("************* Plotting meridional wind [m/s] *************")
	pdf(paste("ED2_",vars[num],"_Met_Drivers.pdf",sep=""),width=14,height=7)	
	par(mfrow=c(1,1),mar=c(4.5,5.2,1,0.8), mgp=c(3,1,0)) # B, L, T, R
	plot(xaxis,vgrd,pch=21,bg="dark grey",col="dark grey",xaxt="n",xlab="Year",
	ylab=expression(paste(Wind[meridional]," (",m~s^{-1},")")),cex=ptcex,
	cex.lab=labcex,cex.axis=axcex)
	axis(1,at=1,label=as.character(years[1]),cex.axis=axcex)			# first year start
	axis(1,at=as.numeric(time_year),label=as.character(years2),cex.axis=axcex)	# remaining years
	abline(v=as.numeric(time_year))
	box(lwd=2.2)
	dev.off() 	# Close figure

	# Shrink PDF
	pdf = paste("ED2_",vars[num],"_Met_Drivers.pdf",sep="")
	png = paste("ED2_",vars[num],"_Met_Drivers.png",sep="")
	system(paste("convert -quality 10 ",pdf," ",png,sep=""))
	system(paste("convert ",png," ",pdf,sep=""))

	############# vddsf
	num=2
	message('')
	message("************* Plotting visible diffuse downward solar radiation [W/m2] *************")
	print("************* Plotting visible diffuse downward solar radiation [W/m2] *************")
	pdf(paste("ED2_",vars[num],"_Met_Drivers.pdf",sep=""),width=14,height=7)	
	par(mfrow=c(1,1),mar=c(4.5,5.2,1,0.8), mgp=c(3,1,0)) # B, L, T, R
	plot(xaxis,vddsf,pch=21,bg="dark grey",col="dark grey",xaxt="n",xlab="Year",
	ylab=expression(paste(Vis.Rad.[diffuse]," (",W~m^{-2},")")),cex=ptcex,
	cex.lab=labcex,cex.axis=axcex)
	axis(1,at=1,label=as.character(years[1]),cex.axis=axcex)			# first year start
	axis(1,at=as.numeric(time_year),label=as.character(years2),cex.axis=axcex)	# remaining years
	abline(v=as.numeric(time_year))
	box(lwd=2.2)
	dev.off() 	# Close figure

	# Shrink PDF
	pdf = paste("ED2_",vars[num],"_Met_Drivers.pdf",sep="")
	png = paste("ED2_",vars[num],"_Met_Drivers.png",sep="")
	system(paste("convert -quality 10 ",pdf," ",png,sep=""))
	system(paste("convert ",png," ",pdf,sep=""))

	############# vbdsf
	num=3
	message('')
	message("************* Plotting visible beam downward solar radiation [W/m2] *************")
	print("************* Plotting visible beam downward solar radiation [W/m2] *************")
	pdf(paste("ED2_",vars[num],"_Met_Drivers.pdf",sep=""),width=14,height=7)	
	par(mfrow=c(1,1),mar=c(4.5,5.2,1,0.8), mgp=c(3,1,0)) # B, L, T, R
	plot(xaxis,vbdsf,pch=21,bg="dark grey",col="dark grey",xaxt="n",xlab="Year",
	ylab=expression(paste(Vis.Rad.[beam]," (",W~m^{-2},")")),cex=ptcex,
	cex.lab=labcex,cex.axis=axcex)
	axis(1,at=1,label=as.character(years[1]),cex.axis=axcex)			# first year start
	axis(1,at=as.numeric(time_year),label=as.character(years2),cex.axis=axcex)	# remaining years
	abline(v=as.numeric(time_year))
	box(lwd=2.2)
	dev.off() 	# Close figure

	# Shrink PDF
	pdf = paste("ED2_",vars[num],"_Met_Drivers.pdf",sep="")
	png = paste("ED2_",vars[num],"_Met_Drivers.png",sep="")
	system(paste("convert -quality 10 ",pdf," ",png,sep=""))
	system(paste("convert ",png," ",pdf,sep=""))

	############# ugrd
	num=4
	message('')
	message("************* Plotting zonal wind [m/s] *************")
	print("************* Plotting zonal wind [m/s] *************")
	pdf(paste("ED2_",vars[num],"_Met_Drivers.pdf",sep=""),width=14,height=7)	
	par(mfrow=c(1,1),mar=c(4.5,5.2,1,0.8), mgp=c(3,1,0)) # B, L, T, R
	plot(xaxis,ugrd,pch=21,bg="dark grey",col="dark grey",xaxt="n",xlab="Year",
	ylab=expression(paste(Wind[zonal]," (",m~s^{-1},")")),cex=ptcex,
	cex.lab=labcex,cex.axis=axcex)
	axis(1,at=1,label=as.character(years[1]),cex.axis=axcex)			# first year start
	axis(1,at=as.numeric(time_year),label=as.character(years2),cex.axis=axcex)	# remaining years
	abline(v=as.numeric(time_year))
	box(lwd=2.2)
	dev.off() 	# Close figure

	# Shrink PDF
	pdf = paste("ED2_",vars[num],"_Met_Drivers.pdf",sep="")
	png = paste("ED2_",vars[num],"_Met_Drivers.png",sep="")
	system(paste("convert -quality 10 ",pdf," ",png,sep=""))
	system(paste("convert ",png," ",pdf,sep=""))

	############# Tair
	num=5
	message('')
	message("************* Plotting air temp (degC) *************")
	print("************* Plotting air temp (degC) *************")
	pdf(paste("ED2_",vars[num],"_Met_Drivers.pdf",sep=""),width=14,height=7)	
	par(mfrow=c(1,1),mar=c(4.5,5.2,1,0.8), mgp=c(3,1,0)) # B, L, T, R
	plot(xaxis,Tair-273.15,pch=21,bg="dark grey",col="dark grey",xaxt="n",xlab="Year",
	ylab=expression(paste(T[air]," (",{degree}," C)" )), cex=ptcex,cex.lab=labcex,cex.axis=axcex)
	axis(1,at=1,label=as.character(years[1]),cex.axis=axcex)			# first year start
	axis(1,at=as.numeric(time_year),label=as.character(years2),cex.axis=axcex)	# remaining years
	abline(v=as.numeric(time_year))
	box(lwd=2.2)
	dev.off()	# Close figure

	# Shrink PDF
	pdf = paste("ED2_",vars[num],"_Met_Drivers.pdf",sep="")
	png = paste("ED2_",vars[num],"_Met_Drivers.png",sep="")
	system(paste("convert -quality 10 ",pdf," ",png,sep=""))
	system(paste("convert ",png," ",pdf,sep=""))

	############# Specific Humidity
	num=6
	message('')
	message("************* Plotting Specific Humidity (kg H20/Kg air) *************")
	print("************* Plotting Specific Humidity (kg H20/Kg air) *************")
	pdf(paste("ED2_",vars[num],"_Met_Drivers.pdf",sep=""),width=14,height=7)	
	par(mfrow=c(1,1),mar=c(4.5,5.2,1,0.8), mgp=c(3,1,0)) # B, L, T, R
	plot(xaxis,sh,pch=21,bg="dark grey",col="dark grey",xaxt="n",xlab="Year",
	ylab=expression(paste("Specific Humidity"," (kg H2O / kg air)" )), cex=ptcex,cex.lab=labcex,cex.axis=axcex)
	axis(1,at=1,label=as.character(years[1]),cex.axis=axcex)			# first year start
	axis(1,at=as.numeric(time_year),label=as.character(years2),cex.axis=axcex)	# remaining years
	abline(v=as.numeric(time_year))
	box(lwd=2.2)
	dev.off()	# Close figure

	# Shrink PDF
	pdf = paste("ED2_",vars[num],"_Met_Drivers.pdf",sep="")
	png = paste("ED2_",vars[num],"_Met_Drivers.png",sep="")
	system(paste("convert -quality 10 ",pdf," ",png,sep=""))
	system(paste("convert ",png," ",pdf,sep=""))
	
	############# pres
	num=7
	message('')
	message("************* Plotting Pressure [Pa] *************")
	print("************* Plotting Pressure [Pa] *************")
	pdf(paste("ED2_",vars[num],"_Met_Drivers.pdf",sep=""),width=14,height=7)	
	par(mfrow=c(1,1),mar=c(4.5,5.2,1,0.8), mgp=c(3,1,0)) # B, L, T, R
	plot(xaxis,pres,pch=21,bg="dark grey",col="dark grey",xaxt="n",xlab="Year",
	ylab="Pressure (Pa)", cex=ptcex,cex.lab=labcex,cex.axis=axcex)
	axis(1,at=1,label=as.character(years[1]),cex.axis=axcex)			# first year start
	axis(1,at=as.numeric(time_year),label=as.character(years2),cex.axis=axcex)	# remaining years
	abline(v=as.numeric(time_year))
	box(lwd=2.2)
	dev.off()	# Close figure

	# Shrink PDF
	pdf = paste("ED2_",vars[num],"_Met_Drivers.pdf",sep="")
	png = paste("ED2_",vars[num],"_Met_Drivers.png",sep="")
	system(paste("convert -quality 10 ",pdf," ",png,sep=""))
	system(paste("convert ",png," ",pdf,sep=""))

	############# Precip rate
	num=8
	message('')
	message("************* Plotting Precip Rate (kg H20/m2/s) *************")
	print("************* Plotting Precip Rate (kg H20/m2/s) *************")
	pdf(paste("ED2_",vars[num],"_Met_Drivers.pdf",sep=""),width=14,height=7)	
	par(mfrow=c(1,1),mar=c(4.5,5.2,1,0.8), mgp=c(3,1,0)) # B, L, T, R
	plot(xaxis,prate,pch=21,bg="dark grey",col="dark grey",xaxt="n",xlab="Year",
	ylab=expression(paste("Precip."," (kg H2O ",m^{-2}~s^{-1},")" )), 
	cex=ptcex,cex.lab=labcex,cex.axis=axcex)
	axis(1,at=1,label=as.character(years[1]),cex.axis=axcex)			# first year start
	axis(1,at=as.numeric(time_year),label=as.character(years2),cex.axis=axcex)	# remaining years
	abline(v=as.numeric(time_year))
	box(lwd=2.2)
	dev.off()	# Close figure

	# Shrink PDF
	pdf = paste("ED2_",vars[num],"_Met_Drivers.pdf",sep="")
	png = paste("ED2_",vars[num],"_Met_Drivers.png",sep="")
	system(paste("convert -quality 10 ",pdf," ",png,sep=""))
	system(paste("convert ",png," ",pdf,sep=""))

	############# cumulative precip
	num=9
	message('')
	message("************* Plotting Cumulative Precip *************")
	print("************* Plotting Cumulative Precip *************")
	pdf(paste("ED2_",vars[num],"_Met_Drivers.pdf",sep=""),width=14,height=7)	
	par(mfrow=c(1,1),mar=c(4.5,5.2,1,0.8), mgp=c(3,1,0)) # B, L, T, R
	plot(xaxis,cumprcp,pch=21,bg="dark grey",col="dark grey",xaxt="n",xlab="Year",
	ylab=expression(paste("Cumulative Precip."," (kg H2O ",m^{-2}~s^{-1},")" )), 
	cex=ptcex,cex.lab=labcex,cex.axis=axcex)
	lines(xaxis,cumprcp,lwd=1.0,col="blue")
	polygon(c(xaxis, xaxis[length(xaxis)]), c(cumprcp, cumprcp[1]), col="blue") 
	axis(1,at=1,label=as.character(years[1]),cex.axis=axcex)			# first year start
	axis(1,at=as.numeric(time_year),label=as.character(years2),cex.axis=axcex)	# remaining years
	abline(v=as.numeric(time_year))
	box(lwd=2.2)
	dev.off()	# Close figure

	# Shrink PDF
	pdf = paste("ED2_",vars[num],"_Met_Drivers.pdf",sep="")
	png = paste("ED2_",vars[num],"_Met_Drivers.png",sep="")
	system(paste("convert -quality 10 ",pdf," ",png,sep=""))
	system(paste("convert ",png," ",pdf,sep=""))

	############# nddsf
	num=10
	message('')
	message("************* Plotting IR diffuse downward solar radiation [W/m2] *************")
	print("************* Plotting IR diffuse downward solar radiation [W/m2] *************")
	pdf(paste("ED2_",vars[num],"_Met_Drivers.pdf",sep=""),width=14,height=7)	
	par(mfrow=c(1,1),mar=c(4.5,5.2,1,0.8), mgp=c(3,1,0)) # B, L, T, R
	plot(xaxis,nddsf,pch=21,bg="dark grey",col="dark grey",xaxt="n",xlab="Year",
	ylab=expression(paste(IR.Rad.[diffuse]," (",W~m^{-2},")")),cex=ptcex,
	cex.lab=labcex,cex.axis=axcex)
	axis(1,at=1,label=as.character(years[1]),cex.axis=axcex)			# first year start
	axis(1,at=as.numeric(time_year),label=as.character(years2),cex.axis=axcex)	# remaining years
	abline(v=as.numeric(time_year))
	box(lwd=2.2)
	dev.off() 	# Close figure

	# Shrink PDF
	pdf = paste("ED2_",vars[num],"_Met_Drivers.pdf",sep="")
	png = paste("ED2_",vars[num],"_Met_Drivers.png",sep="")
	system(paste("convert -quality 10 ",pdf," ",png,sep=""))
	system(paste("convert ",png," ",pdf,sep=""))

	############# nbdsf
	num=11
	message('')
	message("************* Plotting IR direct beam downward solar radiation [W/m2] *************")
	print("************* Plotting IR direct beam downward solar radiation [W/m2] *************")
	pdf(paste("ED2_",vars[num],"_Met_Drivers.pdf",sep=""),width=14,height=7)	
	par(mfrow=c(1,1),mar=c(4.5,5.2,1,0.8), mgp=c(3,1,0)) # B, L, T, R
	plot(xaxis,nbdsf,pch=21,bg="dark grey",col="dark grey",xaxt="n",xlab="Year",
	ylab=expression(paste(IR.Rad.[beam]," (",W~m^{-2},")")),cex=ptcex,
	cex.lab=labcex,cex.axis=axcex)
	axis(1,at=1,label=as.character(years[1]),cex.axis=axcex)			# first year start
	axis(1,at=as.numeric(time_year),label=as.character(years2),cex.axis=axcex)	# remaining years
	abline(v=as.numeric(time_year))
	box(lwd=2.2)
	dev.off() 	# Close figure

	# Shrink PDF
	pdf = paste("ED2_",vars[num],"_Met_Drivers.pdf",sep="")
	png = paste("ED2_",vars[num],"_Met_Drivers.png",sep="")
	system(paste("convert -quality 10 ",pdf," ",png,sep=""))
	system(paste("convert ",png," ",pdf,sep=""))

	############# hgt
	num=12
	message('')
	message("************* Plotting Geopotential Height [m] *************")
	print("************* Plotting Geopotential Height [m] *************")
	pdf(paste("ED2_",vars[num],"_Met_Drivers.pdf",sep=""),width=14,height=7)	
	par(mfrow=c(1,1),mar=c(4.5,5.2,1,0.8), mgp=c(3,1,0)) # B, L, T, R
	plot(xaxis,hgt,pch=21,bg="dark grey",col="dark grey",xaxt="n",xlab="Year",
	ylab="Geopotential Height (m)",cex=ptcex,
	cex.lab=labcex,cex.axis=axcex)
	axis(1,at=1,label=as.character(years[1]),cex.axis=axcex)			# first year start
	axis(1,at=as.numeric(time_year),label=as.character(years2),cex.axis=axcex)	# remaining years
	abline(v=as.numeric(time_year))
	box(lwd=2.2)
	dev.off() 	# Close figure

	# Shrink PDF
	pdf = paste("ED2_",vars[num],"_Met_Drivers.pdf",sep="")
	png = paste("ED2_",vars[num],"_Met_Drivers.png",sep="")
	system(paste("convert -quality 10 ",pdf," ",png,sep=""))
	system(paste("convert ",png," ",pdf,sep=""))

	############# dlwrf
	num=13
	message('')
	message("************* Plotting Downward long wave radiation [W/m2] *************")
	print("************* Plotting Downward long wave radiation [W/m2] *************")
	pdf(paste("ED2_",vars[num],"_Met_Drivers.pdf",sep=""),width=14,height=7)	
	par(mfrow=c(1,1),mar=c(4.5,5.2,1,0.8), mgp=c(3,1,0)) # B, L, T, R
	plot(xaxis,dlwrf,pch=21,bg="dark grey",col="dark grey",xaxt="n",xlab="Year",
	ylab=expression(paste(LW.Rad.[downward]," (",W~m^{-2},")")),cex=ptcex,
	cex.lab=labcex,cex.axis=axcex)
	axis(1,at=1,label=as.character(years[1]),cex.axis=axcex)			# first year start
	axis(1,at=as.numeric(time_year),label=as.character(years2),cex.axis=axcex)	# remaining years
	abline(v=as.numeric(time_year))
	box(lwd=2.2)
	dev.off() 	# Close figure

	# Shrink PDF
	pdf = paste("ED2_",vars[num],"_Met_Drivers.pdf",sep="")
	png = paste("ED2_",vars[num],"_Met_Drivers.png",sep="")
	system(paste("convert -quality 10 ",pdf," ",png,sep=""))
	system(paste("convert ",png," ",pdf,sep=""))

	############# CO2 Concentration
	num=14
	message('')
	message("************* Plotting CO2 Concentration (ppm) *************")
	print("************* Plotting CO2 Concentration (ppm) *************")
	pdf(paste("ED2_",vars[num],"_Met_Drivers.pdf",sep=""),width=14,height=7)	
	par(mfrow=c(1,1),mar=c(4.5,5.2,1,0.8), mgp=c(3,1,0)) # B, L, T, R
	plot(xaxis,CO2,pch=21,bg="dark grey",col="dark grey",xaxt="n",xlab="Year",
	ylab=expression(paste(CO["2"]," Conc. (ppm)",)), 
	cex=ptcex,cex.lab=labcex,cex.axis=axcex)
	axis(1,at=1,label=as.character(years[1]),cex.axis=axcex)			# first year start
	axis(1,at=as.numeric(time_year),label=as.character(years2),cex.axis=axcex)	# remaining years
	abline(v=as.numeric(time_year))
	box(lwd=2.2)	# Close figure
	dev.off()

	# Shrink PDF
	pdf = paste("ED2_",vars[num],"_Met_Drivers.pdf",sep="")
	png = paste("ED2_",vars[num],"_Met_Drivers.png",sep="")
	system(paste("convert -quality 10 ",pdf," ",png,sep=""))
	system(paste("convert ",png," ",pdf,sep=""))

	#------------------String Together PDFs and remove individual .pdfs------------------------#
	# Info:  could string together pngs into single pdf
	out_pdf=paste("ED2_Met_Driver_Diagnostics.pdf",sep="")
	try(system(paste("rm ",out_pdf)),silent=TRUE)
	try(system(paste("gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=",
			out_pdf," *.pdf",sep="")),silent=FALSE)
	system("rm *_Drivers.pdf")	# Delete individual .pdfs, leave concatentated file

} # END OF IF/THEN
#--------------------------------------------------------------------------------------------------#


#---------------- Script complete -----------------------------------------------------------------#
print("*********************************************************")
print("**************** PROCESSING COMPLETE! *******************")
print("*********************************************************")


####################################################################################################
### EOF
####################################################################################################
