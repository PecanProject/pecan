#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
####################################################################################################
#               Driver script for PEcAn ED2 diagnostic plots
#               -- V1.  Called by bash script "pecan.ed2.diagnostics.sh"
#               -- Use differet output based on requested plots.  E.g. tower files
#                  for fluxes, daily for fluxes, LAI, etc.  Check to see what types of outputs
#                  were created for model run and generate various plots.  E.g. no -D-
#                  output then only plot -M-, -Y-, or -T- data. Needs to become flexible
#
#               -- TODO: Allow choice of which output to plot?  Make flexbile based on input
#               -- TODO: Include some dynamic time options. E.g. Day, month, year on X-axis 
#                  for temporal plots?
#               -- Allow for bi-variate plots?  Maybe add this as another function/script file?
#               -- TODO: Clean up and document code.              
####################################################################################################


#---------------- Close all devices and delete all variables. -------------------------------------#
rm(list=ls(all=TRUE)) # clear workspace
graphics.off()  # close any open graphics
#--------------------------------------------------------------------------------------------------#


#---------------- Load required libraries ---------------------------------------------------------#
# Info: Loads required R libraries and warns if package is not availible.
ok = require(chron); if (! ok) stop("Package chron is not available...")
ok = require(hdf5) ; if (! ok) stop("Package hdf5 is not available...")
ok = require(XML) ; if (! ok) stop("Package XML is not available...")
#--------------------------------------------------------------------------------------------------#


#---------------- Import command arguments from shell ---------------------------------------------#
args = commandArgs(trailingOnly = TRUE) # import any needed arguments for the terminal
print(args)   			#---> Print command arguments in R output. For debugging.
pecan.home <- Sys.getenv("PECANHOME") #<--- Import PEcAn home directory

# ED2 diagnostic plot functions
source(paste(pecan.home,"rscripts/pecan.ed2.diag.plots.R",sep=""))
# Time utilities.  Stolen from Harvard group.
source(paste(pecan.home,"rscripts/timeutils.R",sep=""))
#--------------------------------------------------------------------------------------------------#


#---------------- Get model run location ----------------------------------------------------------#
if (args[1]=="pwd"){
  model_run=paste(getwd(),"/",sep="")	# if set as pwd
}else{
  model_run=paste(args[1],"/",sep="")	# if full path given
}
#--------------------------------------------------------------------------------------------------#


#---------------- Setup script output(s) ----------------------------------------------------------#
# Info: Generate output folder for diagnostic plots.  Will create folder if it doesn't already
#   exist.
output_dir = paste(model_run,"/ED2_Diagnostics/",sep="")
if (! file.exists(output_dir)) dir.create(output_dir)
#--------------------------------------------------------------------------------------------------#


#---------------- Read ED2IN or Config File -------------------------------------------------------#
print('******************** Reading ED2IN File *********************')
# Info: SET THIS PART UP!
#if (args[]=="xml"){  
#	}else{	
#}

# ED2IN filename
if (args[2]=="-f"){
  ED2IN_fn = grep(list.files(),pattern='ED2IN',value=TRUE)
}else{
  ED2IN_fn = args[2]
}

#---- Import ED2IN file
ED2IN = readLines(ED2IN_fn)
#--------------------------------------------------------------------------------------------------#


#---------------- Get Run info From ED2IN File ----------------------------------------------------#
# Info: INFO HERE

# Temporarily turn off warning messages when readin ED2IN
options(warn=-1)

#---- Get run type (NL%RUNTYPE)
RUNTYP = grep(ED2IN,pattern='NL%RUNTYPE',value=TRUE)
indices = gregexpr("'", RUNTYP)[[1]]
RUNTYP = substr(RUNTYP,indices[1]+1, indices[2]-1)
RUNTYP = RUNTYP[1]

#---- Get run ID (NL%EXPNME)
RUNID = grep(ED2IN,pattern='NL%EXPNME',value=TRUE)
indices = gregexpr("'", RUNID)[[1]]
RUNID = substr(RUNID,indices[1]+1, indices[2]-1)

#---- Get run length info.  Start month.  Can also get this from XML file
IMONTHA = grep(ED2IN,pattern='NL%IMONTHA',value=TRUE)
indices = gregexpr("[0-9]", IMONTHA)[[1]]
IMONTHA = substr(IMONTHA,indices[1], indices[2])

#---- Get start date
IDATEA = grep(ED2IN,pattern='NL%IDATEA',value=TRUE)
indices = gregexpr("[0-9]", IDATEA)[[1]]
IDATEA = substr(IDATEA,indices[1], indices[2])

#---- Get start year
IYEARA = grep(ED2IN,pattern='NL%IYEARA',value=TRUE)
indices = gregexpr("[0-9][0-9][0-9][0-9]", IYEARA)[[1]]
IYEARA = substr(IYEARA,indices[1], indices[1]+4)

#---- Get final month
IMONTHZ = grep(ED2IN,pattern='NL%IMONTHZ',value=TRUE)
indices = gregexpr("[0-9]", IMONTHZ)[[1]]
IMONTHZ = substr(IMONTHZ,indices[1], indices[2])

#---- Get final day
IDATEZ = grep(ED2IN,pattern='NL%IDATEZ',value=TRUE)
indices = gregexpr("[0-9]", IDATEZ)[[1]]
IDATEZ = substr(IDATEZ,indices[1], indices[2])

#---- Get final year
IYEARZ = grep(ED2IN,pattern='NL%IYEARZ',value=TRUE)
indices = gregexpr("[0-9][0-9][0-9][0-9]", IYEARZ)[[1]]
IYEARZ = substr(IYEARZ,indices[1], indices[1]+4)

#---- Get location info (NL%POI_LAT,NLPOI_LON). Prob better to get from XML file
POI_LAT = grep(ED2IN,pattern='NL%[PS]OI_LAT',value=TRUE)
indices = gregexpr("[0-9]", POI_LAT)[[1]]
POI_LAT = substr(POI_LAT,indices[1], indices[length(indices)])

POI_LON = grep(ED2IN,pattern='NL%[PS]OI_LON',value=TRUE)
neg1 = gregexpr("-", POI_LON)[[1]]
if (neg1==-1){
  indices = gregexpr("[0-9]", POI_LON)[[1]]
  POI_LON = substr(POI_LON,indices[1], indices[length(indices)])
}else{
  neg2 = substr(POI_LON,neg1[1],neg1[1])
  indices = gregexpr("[0-9]", POI_LON)[[1]]
  POI_LON = substr(POI_LON,indices[1], indices[length(indices)])
  POI_LON = paste(neg2,POI_LON,sep="")
}

#---- Get output type settings (i.e. I, D, E, Y, T)
#---- Site average inst.
IFOUTPUT = grep(ED2IN,pattern='NL%IFOUTPUT',value=TRUE)
indices = gregexpr("[0-9]", IFOUTPUT)[[1]]
IFOUTPUT = substr(IFOUTPUT,indices[1], indices[length(indices)])
IFOUTPUT=as.numeric(IFOUTPUT)
IFOUTPUT[IFOUTPUT==0]="No"
IFOUTPUT[IFOUTPUT==0]="Yes"

#---- Daily mean output
IDOUTPUT = grep(ED2IN,pattern='NL%IDOUTPUT',value=TRUE)
indices = gregexpr("[0-9]", IDOUTPUT)[[1]]
IDOUTPUT = substr(IDOUTPUT,indices[1], indices[length(indices)])
IDOUTPUT=as.numeric(IDOUTPUT)
IDOUTPUT[IDOUTPUT==0]="No"
IDOUTPUT[IDOUTPUT==3]="Yes"

#---- Monthly mean output
IMOUTPUT = grep(ED2IN,pattern='NL%IMOUTPUT',value=TRUE)
indices = gregexpr("[0-9]", IMOUTPUT)[[1]]
IMOUTPUT = substr(IMOUTPUT,indices[1], indices[length(indices)])
IMOUTPUT=as.numeric(IMOUTPUT)
IMOUTPUT[IMOUTPUT==0]="No"
IMOUTPUT[IMOUTPUT==3]="Yes"

#---- Site average flux files
ITOUTPUT = grep(ED2IN,pattern='NL%ITOUTPUT',value=TRUE)
indices = gregexpr("[0-9]", ITOUTPUT)[[1]]
ITOUTPUT = substr(ITOUTPUT,indices[1], indices[length(indices)])
ITOUTPUT=as.numeric(ITOUTPUT)
ITOUTPUT[ITOUTPUT==0]="No"
ITOUTPUT[ITOUTPUT==3]="Yes"

#---- Get output frequency (NL%FRQFAST)
FRQFAST = grep(ED2IN,pattern='NL%FRQFAST',value=TRUE)
indices = gregexpr("[0-9]", FRQFAST)[[1]]
FRQFAST = substr(FRQFAST,indices[1], indices[length(indices)])
FRQFAST=as.numeric(FRQFAST)

#---- Get output file locations (NL%FFILOUT & NL%SFILOUT).  Can also get this from XML file
FFILOUT = grep(ED2IN,pattern='NL%FFILOUT',value=TRUE)
indices = gregexpr("'", FFILOUT)[[1]]
FFILOUT = substr(FFILOUT,indices[1]+1, indices[2]-1)
indices = tail(gregexpr("/", FFILOUT)[[1]], n=1)
FFILOUT = substr(FFILOUT,1,indices)

#---- Get soil flag (NL%ISOILFLG)
ISOILFLG = grep(ED2IN,pattern='NL%ISOILFLG',value=TRUE)
indices = gregexpr("[0-9]", ISOILFLG)[[1]]
ISOILFLG = substr(ISOILFLG,indices[1], indices[1])

#---- Get number of soil layers (NL%NZG)
NZG = grep(ED2IN,pattern='NL%NZG',value=TRUE)
indices = gregexpr("[0-9]", NZG)[[1]]
NZG = substr(NZG,indices[1], indices[1])

#---- Get prescribed fraction of sand and clay (NL%SLXCLAY & NL%SLXSAND)
if (ISOILFLG=="2"){
  SLXCLAY = grep(ED2IN,pattern='NL%SLXCLAY',value=TRUE)
  indices = gregexpr("[0.0-1.0]", SLXCLAY)[[1]]
  SLXCLAY = substr(SLXCLAY,indices[1], indices[1]+3)
  
  SLXSAND = grep(ED2IN,pattern='NL%SLXSAND',value=TRUE)
  indices = gregexpr("[0.0-1.0]", SLXSAND)[[1]]
  SLXSAND = substr(SLXSAND,indices[1], indices[1]+3)
}

# TODO: Could export more info from ED2IN or eventually XML file here.  Useful for review in log
# file

# Turn back on warning messages.  Off for irrelevant errors when parsing ED2IN file
options(warn=0)
#--------------------------------------------------------------------------------------------------#


#---------------- Display run info to the screen --------------------------------------------------#
message('')
message("*********************************************************")
message("---- ED2 Run Info ----")
message("*********************************************************")
message('')
message(paste("---- ED2IN: ",ED2IN_fn))
message(paste("---- Run Type: ",RUNTYP))
message(paste("---- Run ID: ",RUNID))
message(paste("---- Run Start: ",IMONTHA,"/",IDATEA,"/",IYEARA,sep=""))
message(paste("---- Run End: ",IMONTHZ,"/",IDATEZ,"/",IYEARZ,sep=""))
message(paste("---- Run Location: ",POI_LAT," Latitude, ",
            POI_LON," Longitude",sep=""))
message(paste("---- ED2 Model Output Directory: ",model_run,FFILOUT,sep=""))
message(paste("---- Instantaneous Output: ",IFOUTPUT,sep=""))
message(paste("---- Daily Mean Output: ",IDOUTPUT,sep=""))
message(paste("---- Monthly Mean Output: ",IMOUTPUT,sep=""))
message(paste("---- Instantaneous Fluxes Output: ",ITOUTPUT,sep=""))
message(paste("---- Output Frequency: ",FRQFAST,"s",sep=""))
message('')
message("---------------------------------------------------------")
message('')
message(paste("---- Soil Layers: ",NZG,sep=""))
message(paste("---- Soil Clay Frac.: ", SLXCLAY,sep=""))
message(paste("---- Soil Sand Frac.: ", SLXSAND,sep=""))
message('')
message("*********************************************************")

# can print more info here

Sys.sleep(2) # pause for 2 seconds
#--------------------------------------------------------------------------------------------------#


#---------------- Setup output for diagnostic plot ------------------------------------------------#
# Info: Get the time info for the run.  There are probably things here that can be elliminated. 

####### First and last day to include in the plots (MM/DD/YYYY). #######
start_date  = as.Date(paste(as.numeric(IYEARA),"/",as.numeric(IMONTHA),
                           "/",as.numeric(IDATEA),sep=""),format="%Y/%m/%d")
start_year  = format(start_date, "%Y")
start_month = format(start_date, "%m")
start_day   = format(start_date, "%d")
start       = paste(start_month,"/",start_day,"/",start_year,sep="")

end_date    = as.Date(paste(as.numeric(IYEARZ),"/",as.numeric(IMONTHZ),
                            "/",as.numeric(IDATEZ),sep=""),format="%Y/%m/%d")
end_year    = format(end_date, "%Y")
end_month   = format(end_date, "%m")
end_day     = format(end_date, "%d")
end         = paste(end_month,"/",end_day,"/",end_year,sep="")

out_day   = 86400/FRQFAST
deltaT   	= 24/(86400/FRQFAST)	# ---> sets the number of obs per day based on FRQFAST
daterange	= seq(from=as.numeric(chron(start)),to=as.numeric(chron(end)),by=deltaT/24)
daterange	= chron(daterange)
n.range 	= length(daterange)

n.months    = (as.numeric(IYEARZ)-as.numeric(IYEARA)-1)*12+
  as.numeric(IMONTHZ)+(12-as.numeric(IMONTHA)+1)
list.mths   = nummonths(daterange)
list.days   = numdays(daterange)
list.mins   = minutes(daterange)
frac        = hms2frac(daterange)
days        = days(daterange)
times1      = rep(seq(0.0,23.5,0.5),each=1,times=1)
times2      = rep(seq(0.5,24,0.5),each=1,times=1)
dates       = data.frame(Date=as.Date(daterange),mon=list.mths,doy=list.days,
                         fjday=frac)
#--------------------------------------------------------------------------------------------------#


#---------------- Call plot functions based on setup ----------------------------------------------#
# Info: MORE INFO HERE

message('')
message('')
model_run
FFILOUT
analysis = FFILOUT #paste(model_run,FFILOUT,sep="")

if (ITOUTPUT=="Yes"){
  message('---- Plotting Site Averaged Fluxes (ITOUTPUT) ----')
  site_fluxes(model_run,analysis,output_dir)
} # END ITOUTPUT

message('')
#if (IDOUTPUT=="Yes"){
#  message('---- Plotting Mean Daily (IDOUTPUT) ----')
#  plot_daily(model_run,analysis,output_dir)
#}
  
  
#}

#if (diel==TRUE){
  # PLOT AVG DIEL CYCLE FOR SUMMER/WINTER
#}

#message('')
#if (IMOUTPUT=="Yes"){
#  message('---- Plotting Mean Monthly (IMOUTPUT) ----')
#  plot_monthly(model_run,analysis,output_dir)
#}

# PUT CODE HERE.  Egs. plot_fast.r, plot_monthly.r, etc

#--------------------------------------------------------------------------------------------------#


#---------------- Script complete -----------------------------------------------------------------#
message('')
message("*********************************************************")
message("**************** PROCESSING COMPLETE! *******************")
message("*********************************************************")
####################################################################################################
### EOF
####################################################################################################
