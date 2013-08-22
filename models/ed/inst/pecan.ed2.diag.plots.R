#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
######################################################################################################
# Plot functions for PEcAn ED2 Diagnostics
# 
# v1
#
# TODO: Finalize plots for various functions
#
######################################################################################################


#====================================================================================================#
# Plot mean daily output
#====================================================================================================#
#  UNDER DEVELOPMENT
plot_daily = function(model.run,in.dir,out.dir){
  
  i = 1
  for (year in start_year:end_year) {
    
    message(paste("--- PROCESSING YEAR: ",year," ---"))
    
    #---------------- Generate Subset Length --------------------------------------------------------#
    if (year == start_year) {
      start_day <- as.numeric(format(start_date, "%j"))
    } else {
      start_day = 1
    }
    if (year == end_year) {
      end_day = as.numeric(format(end_date, "%j"))
    } else {
      end_day = as.numeric(format(as.Date(sprintf("%s-12-31", year)), "%j"))
    }
    
  } 
  
  
} # End of plot_daily
#----------------------------------------------------------------------------------------------------#
  

#====================================================================================================#
# Plot mean diel function
#====================================================================================================#
# not implemented yet
#----------------------------------------------------------------------------------------------------#


#====================================================================================================#
# Plot site average fluxes (i.e. "Tower" file output)
#====================================================================================================#

site_fluxes = function(model.run,in.dir,out.dir){

  #---------------- Import prescribed pheno data, if exists -----------------------------------------#
  # Info: Display prescribed phenology on diagnostic plots (if present) 
  # May need to get rid of this as it is mostly ED specific
  pheno = list.files(path=model.run,pattern="phenology")
  if (length(pheno)==0) {
    site_pheno=NA
  }else{
    pheno_data = read.delim(pheno,header=F,sep="\t",skip=1)
    Yr = pheno_data[,1]
    GU = 1/pheno_data[,2]
    LO = 1/pheno_data[,4]
    site_pheno = data.frame(Year=Yr,Greenup=GU,LeafOff=LO)
    print('Site Phenology Info (DoY)')
    print(site_pheno)
    print("")
  }
  #--------------------------------------------------------------------------------------------------#
  
  i = 1
  for (year in start_year:end_year) {
    message(paste("--- PROCESSING YEAR: ",year," ---"))
    
    #---------------- Generate Subset Length --------------------------------------------------------#
    if (year == start_year) {
      start_day <- as.numeric(format(start_date, "%j"))
    } else {
      start_day = 1
    }
    if (year == end_year) {
      end_day = as.numeric(format(end_date, "%j"))
    } else {
      end_day = as.numeric(format(as.Date(sprintf("%s-12-31", year)), "%j"))
    }
    
    polyx = start_day:end_day # <--- for plotting below
    vals_day    = out_day     # <--- values written out per day, 86400/FRQFAST
    hdflength   = (vals_day*(1+end_day-start_day))
    
    #---------------- Init. Arrays ------------------------------------------------------------------#
    # Info: Initialize arrays for entire model run and populate with for loop (below)
    GPP.AVG               = rep(0,times=hdflength)
    VLEAF.RESP.AVG        = rep(0,times=hdflength)
    LEAF.RESP.AVG 	      = rep(0,times=hdflength) 
    STORAGE.RESP.AVG	    = rep(0,times=hdflength)
    GROWTH.RESP.AVG	      = rep(0,times=hdflength)
    ROOT.RESP.AVG		      = rep(0,times=hdflength)
    PLANT.RESP.AVG   	    = rep(0,times=hdflength)
    HTROPH.RESP.AVG	      = rep(0,times=hdflength)
    Reco.AVG		          = rep(0,times=hdflength)
    NPP.AVG		            = rep(0,times=hdflength)
    NEE.AVG      		      = rep(0,times=hdflength)
    #---------------------------------------------
    # Units: [kg/m2/s]
    #AVG.VAPOR.WC          = rep(0,times=hdflength) # wood vapor flux.  
    AVG.VAPOR.LC          = rep(0,times=hdflength)
    AVG.VAPOR.GC          = rep(0,times=hdflength)
    AVG.VAPOR.AC          = rep(0,times=hdflength)
    AVG.TRANSP            = rep(0,times=hdflength)
    AVG.EVAP              = rep(0,times=hdflength)
    # Units [kg/kg]
    AVG.CAN.SHV           = rep(0,times=hdflength)
    #---------------------------------------------
    # Not implemented yet
    #AVG.SOIL.TEMP		      = rep(0,times=hdflength)
    #CAN.AIR.TEMP.AVG	    = rep(0,times=hdflength)
    #SWC.AVG		            = rep(0,times=hdflength)
    #AVG.SFCWATER.DEPTH    = rep(0,times=hdflength)
    #------------------------------------------------------------------------------------------------#
    
    
    #------------------------------------------------------------------------------------------------#
    # Info from driver script
    # dates contains YYmmdd, month (num), doy. fjday (0-1)  
    init    = dates[1,4]
    total   = seq(1,hdflength,1) # <--- is this unused?
    reps    = hdflength/vals_day # <--- this should set the total number of days of data based on
				                         # hdf length.  E.g. 48 obs per day -- 17520/48 = 365
    dayfrac = rep(seq(deltaT,24,deltaT), each=1, times=reps) # <--- setup daily output rate for subset
                                                             # rep over total lenght (hdflength/vals) 
    subset    = 0 # <--- initialize variable
   
    period = c(10.0,17.0) # <--- choose which times to average over.  Can make user selectable.
     
    s      = seq(period[1],period[2],deltaT)
    subset = which(dayfrac >= period[1] & dayfrac <= period[2])
    hours = dayfrac[dayfrac >= period[2] & dayfrac <= period[1]] 
    aggrlist = rep(start_day:(end_day), each=length(s))   # subset list

    
    #---------------- Load ED2 Model Output (hdf5) --------------------------------------------------#
    filename = list.files(in.dir,full.names=TRUE,
                          pattern=paste('.*-T-', year, '-.*.h5', sep=''))[1]
    if (is.na(filename)==1) {
      break
    }else{
      data <- hdf5load(filename, load = FALSE,tidy=TRUE) # LOAD ED2 OUTPUT
    }
    var_names = summary(data) # View info about vars. For debugging
    if (i==1){
      print(paste("Site Averaged Fluxes (ITOUTPUT) for ",year))
      print(var_names) # Show variable names in log file
      print("")
      #print(str(data))
    }
    i=i+1
    #------------------------------------------------------------------------------------------------#
    
    
    #---------------- Get Phenology Information -----------------------------------------------------#
    chk = which(site_pheno==year)
    if (is.nan(mean(chk))==1) {
      phenology = data.frame(-9999,-9999,-9999)
      names(phenology)=c("Year","Greenup","LeafOff")
      GS_LENGTH = NA
    }else{
      phenology = site_pheno[chk,]
      GS_LENGTH = phenology[,3]-phenology[,2]
    }
    #------------------------------------------------------------------------------------------------#
    
    
    #---------------- Generate Figures --------------------------------------------------------------#
    umol2gc <- 1.0368 # convert to gC
    ######################## SETUP PLOT PARAMETERS ###################################################
    cex     = 1  
    labcex  = 2
    axiscex = 2
    maincex = 2
    linew   = 1.3 # line width
    ######################## ED2 OUTPUT ##############################################################
    # units: umol/m2/s
    GPP.AVG       	        = data$AVG.GPP[subset]*umol2gc
    GPP.AVG.mn              = aggregate(GPP.AVG,by=list(aggrlist),mean)[[2]]
    GPP.AVG.ll              = aggregate(GPP.AVG,by=list(aggrlist),min)[[2]]
    GPP.AVG.ul              = aggregate(GPP.AVG,by=list(aggrlist),max)[[2]]
    #------------------------------------------------------------------------------------------------#
    # units: umol/m2/s
    LEAF.RESP.AVG	          = data$AVG.LEAF.RESP[subset]*umol2gc
    LEAF.RESP.AVG.mn        = aggregate(LEAF.RESP.AVG,by=list(aggrlist),mean)[[2]]
    LEAF.RESP.AVG.ll        = aggregate(LEAF.RESP.AVG,by=list(aggrlist),min)[[2]]
    LEAF.RESP.AVG.ul        = aggregate(LEAF.RESP.AVG,by=list(aggrlist),max)[[2]]
    #------------------------------------------------------------------------------------------------#
    # units: umol/m2/s
    VLEAF.RESP.AVG	        = data$AVG.VLEAF.RESP[subset]*umol2gc
    VLEAF.RESP.AVG.mn       = aggregate(VLEAF.RESP.AVG,by=list(aggrlist),mean)[[2]]
    VLEAF.RESP.AVG.ll      = aggregate(VLEAF.RESP.AVG,by=list(aggrlist),min)[[2]]
    VLEAF.RESP.AVG.ul      = aggregate(VLEAF.RESP.AVG,by=list(aggrlist),max)[[2]]
    #------------------------------------------------------------------------------------------------#
    # units: umol/m2/s
    STORAGE.RESP.AVG	      = data$AVG.STORAGE.RESP[subset]*umol2gc
    STORAGE.RESP.AVG.mn       = aggregate(STORAGE.RESP.AVG,by=list(aggrlist),mean)[[2]]
    STORAGE.RESP.AVG.ll       = aggregate(STORAGE.RESP.AVG,by=list(aggrlist),min)[[2]]
    STORAGE.RESP.AVG.ul       = aggregate(STORAGE.RESP.AVG,by=list(aggrlist),max)[[2]]
    #------------------------------------------------------------------------------------------------#
    # units: umol/m2/s
    GROWTH.RESP.AVG	        = data$AVG.GROWTH.RESP[subset]*umol2gc
    GROWTH.RESP.AVG.mn       = aggregate(GROWTH.RESP.AVG,by=list(aggrlist),mean)[[2]]
    GROWTH.RESP.AVG.ll       = aggregate(GROWTH.RESP.AVG,by=list(aggrlist),min)[[2]]
    GROWTH.RESP.AVG.ul       = aggregate(GROWTH.RESP.AVG,by=list(aggrlist),max)[[2]]
    #------------------------------------------------------------------------------------------------#
    # units: umol/m2/s
    ROOT.RESP.AVG	         = data$AVG.ROOT.RESP[subset]*umol2gc
    ROOT.RESP.AVG.mn       = aggregate(ROOT.RESP.AVG,by=list(aggrlist),mean)[[2]]
    ROOT.RESP.AVG.ll       = aggregate(ROOT.RESP.AVG,by=list(aggrlist),min)[[2]]
    ROOT.RESP.AVG.ul       = aggregate(ROOT.RESP.AVG,by=list(aggrlist),max)[[2]]
    #------------------------------------------------------------------------------------------------#
    PLANT.RESP.AVG  	      = data$AVG.PLANT.RESP[subset] *umol2gc 
    PLANT.RESP.AVG.mn       = aggregate(PLANT.RESP.AVG,by=list(aggrlist),mean)[[2]]
    PLANT.RESP.AVG.ll       = aggregate(PLANT.RESP.AVG,by=list(aggrlist),min)[[2]]
    PLANT.RESP.AVG.ul       = aggregate(PLANT.RESP.AVG,by=list(aggrlist),max)[[2]]
    #------------------------------------------------------------------------------------------------#
    HTROPH.RESP.AVG 	      = data$AVG.HTROPH.RESP[subset] *umol2gc
    HTROPH.RESP.AVG.mn      = aggregate(HTROPH.RESP.AVG,by=list(aggrlist),mean)[[2]]
    HTROPH.RESP.AVG.ll      = aggregate(HTROPH.RESP.AVG,by=list(aggrlist),min)[[2]]
    HTROPH.RESP.AVG.ul      = aggregate(HTROPH.RESP.AVG,by=list(aggrlist),max)[[2]]
    #------------------------------------------------------------------------------------------------#
    Reco.AVG.mn	            = (PLANT.RESP.AVG.mn + HTROPH.RESP.AVG.mn) 
    Reco.AVG.ll             = (PLANT.RESP.AVG.ll + HTROPH.RESP.AVG.ll)
    Reco.AVG.ul             = (PLANT.RESP.AVG.ul + HTROPH.RESP.AVG.ul)
    #------------------------------------------------------------------------------------------------#
    #NPP.AVG                 = data$AVG.NPPDAILY[subset] *umol2gc
    #NPP.AVG.mn              = aggregate(NPP.AVG,by=list(aggrlist),mean)[[2]]
    #NPP.AVG.ll              = aggregate(NPP.AVG,by=list(aggrlist),min)[[2]]
    #NPP.AVG.ul              = aggregate(NPP.AVG,by=list(aggrlist),max)[[2]]
    NPP.AVG.mn        	    = (GPP.AVG.mn - PLANT.RESP.AVG.mn) 
    NPP.AVG.ll              = (GPP.AVG.ll - PLANT.RESP.AVG.ul)
    NPP.AVG.ul              = (GPP.AVG.ul - PLANT.RESP.AVG.ll)
    #------------------------------------------------------------------------------------------------#
    NEE.AVG.mn         	    = -1*(GPP.AVG.mn - (PLANT.RESP.AVG.mn + HTROPH.RESP.AVG.mn)) 
    NEE.AVG.ll              = -1*(GPP.AVG.ll - (PLANT.RESP.AVG.ul + HTROPH.RESP.AVG.ul))
    NEE.AVG.ul              = -1*(GPP.AVG.ul - (PLANT.RESP.AVG.ll + HTROPH.RESP.AVG.ll))
    #------------------------------------------------------------------------------------------------#
    # [kg/m2/s]
    #AVG.VAPOR.WC            = data$AVG.VAPOR.WC[subset] #polygon wood to canopy air vapor flux
    #AVG.VAPOR.WC.mn         = aggregate(AVG.VAPOR.WC,by=list(aggrlist),mean)[[2]]
    #AVG.VAPOR.WC.ll         = aggregate(AVG.VAPOR.WC,by=list(aggrlist),min)[[2]]
    #AVG.VAPOR.WC.ul         = aggregate(AVG.VAPOR.WC,by=list(aggrlist),max)[[2]]
    #------------------------------------------------------------------------------------------------#
    # attempt to make this backwards compatible
    AVG.VAPOR.LC            = tryCatch(data$AVG.VAPOR.LC[subset],finally= data$AVG.VAPOR.VC[subset])
    AVG.VAPOR.LC.mn         = aggregate(AVG.VAPOR.LC,by=list(aggrlist),mean)[[2]]
    AVG.VAPOR.LC.ll         = aggregate(AVG.VAPOR.LC,by=list(aggrlist),min)[[2]]
    AVG.VAPOR.LC.ul         = aggregate(AVG.VAPOR.LC,by=list(aggrlist),max)[[2]]
    #------------------------------------------------------------------------------------------------#
    AVG.VAPOR.GC            = data$AVG.VAPOR.GC[subset] #polygon moisture flux ground to canopy air
    AVG.VAPOR.GC.mn         = aggregate(AVG.VAPOR.GC,by=list(aggrlist),mean)[[2]]
    AVG.VAPOR.GC.ll         = aggregate(AVG.VAPOR.GC,by=list(aggrlist),min)[[2]]
    AVG.VAPOR.GC.ul         = aggregate(AVG.VAPOR.GC,by=list(aggrlist),max)[[2]]
    #------------------------------------------------------------------------------------------------#
    AVG.VAPOR.AC            = data$AVG.VAPOR.AC[subset]#polygon vapor flux atmosphere to canopy air
    AVG.VAPOR.AC.mn         = aggregate(AVG.VAPOR.AC,by=list(aggrlist),mean)[[2]]
    AVG.VAPOR.AC.ll         = aggregate(AVG.VAPOR.AC,by=list(aggrlist),min)[[2]]
    AVG.VAPOR.AC.ul         = aggregate(AVG.VAPOR.AC,by=list(aggrlist),max)[[2]]                               
    #------------------------------------------------------------------------------------------------#    
    AVG.TRANSP              = data$AVG.TRANSP[subset]#polygon transpiration from stomata to canopy air spac
    AVG.TRANSP.mn           = aggregate(AVG.TRANSP,by=list(aggrlist),mean)[[2]]
    AVG.TRANSP.ll           = aggregate(AVG.TRANSP,by=list(aggrlist),min)[[2]]
    AVG.TRANSP.ul           = aggregate(AVG.TRANSP,by=list(aggrlist),max)[[2]]
    #------------------------------------------------------------------------------------------------#
    AVG.EVAP                = data$AVG.EVAP[subset] #Polygon averaged evap/dew from ground and leaves to C
    AVG.EVAP.mn             = aggregate(AVG.EVAP,by=list(aggrlist),mean)[[2]]
    AVG.EVAP.ll             = aggregate(AVG.EVAP,by=list(aggrlist),min)[[2]]
    AVG.EVAP.ul             = aggregate(AVG.EVAP,by=list(aggrlist),max)[[2]]
    #------------------------------------------------------------------------------------------------#
    AVG.CAN.SHV                = data$AVG.CAN.SHV[subset] #Polygon Average Specific Humidity of Canopy Air
    AVG.CAN.SHV.mn             = aggregate(AVG.CAN.SHV,by=list(aggrlist),mean)[[2]]
    AVG.CAN.SHV.ll             = aggregate(AVG.CAN.SHV,by=list(aggrlist),min)[[2]]
    AVG.CAN.SHV.ul             = aggregate(AVG.CAN.SHV,by=list(aggrlist),max)[[2]]
    #------------------------------------------------------------------------------------------------#
    #AVG.SOIL.TEMP            = data$AVG.SOIL.TEMP[subset,1,9]-273.15 #Polygon Average Soil Temperature
    #AVG.SOIL.TEMP.5cm       = aggregate(AVG.SOIL.TEMP,by=list(aggrlist),mean)[[2]]
    #AVG.SOIL.TEMP            = data$AVG.SOIL.TEMP[subset,1,8]-273.15 #Polygon Average Soil Temperature
    #AVG.SOIL.TEMP.10cm       = aggregate(AVG.SOIL.TEMP,by=list(aggrlist),mean)[[2]]
    #------------------------------------------------------------------------------------------------#
    #CAN.AIR.TEMP.AVG	= (data$AVG.CAN.TEMP[subset])-273.15 # convert to celcius
    #SWC.AVG			= data$AVG.SOIL.WATER[subset,1,9] # soil moisture at 5cm  
    ###########################################################################################################
    ##################################### COMPONENT FLUXES ####################################################
    pdf(paste(out.dir,"/","ED2_",year,"_Site_Avg_Fluxes.pdf",sep=""),width=12,height=11,
        onefile=TRUE)
    par(mfrow=c(3,2),mar=c(5,5.7,0.9,0.5),mgp=c(3.3,1.5,0),oma=c(0,0,3,0)) # B, L, T, R
    
    #==========================================================================================================
    # GPP
    #==========================================================================================================
    ylim = range(c(GPP.AVG.ll,GPP.AVG.ul),na.rm=TRUE) # define Y lims
    plot(start_day:end_day,GPP.AVG.mn,xlab='',ylab=expression(paste(GPP," (gC",~m^{-2},")")),
         ylim=ylim,pch=21,col="black", bg="black",
         cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(v=phenology[,2],lty=2,lwd=1.5,col="green3")
    abline(v=phenology[,3],lty=2,lwd=1.5,col="brown")
    polygon(c(polyx, rev(polyx)), c(GPP.AVG.ul, rev(GPP.AVG.ll)), col="light gray", border="dark grey",lty=2)  
    lines(start_day:end_day,GPP.AVG.mn,lty=1,col="black")
    points(start_day:end_day,GPP.AVG.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    if (is.nan(mean(chk))==0) {
      legend("topleft",legend=c("Greenup","Leaf Off"),bty="n",
             lty=2,lwd=1.5,col=c("green3","brown"),cex=2)
      #text(37,max(GPP.AVG)-4,"GS Length:",cex=2)
      #text(35,max(GPP.AVG)-5,paste(round(GS_LENGTH,2)," days",sep=""),
      #    cex=2 )
    }
    abline(h=0,lty=2,lwd=1.5,col="black")
    rm(chk)
    box(lwd=2.2)
    #==========================================================================================================
    # NPP
    #==========================================================================================================
    ylim = range(c(NPP.AVG.ll,NPP.AVG.ul),na.rm=TRUE) # define Y lims
    plot(start_day:end_day,NPP.AVG.mn,xlab='',ylab=expression(paste(NPP," (gC",~m^{-2},")")),
         pch=21,col="black", bg="black",ylim=ylim,
         cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx, rev(polyx)), c(NPP.AVG.ul, rev(NPP.AVG.ll)), 
            col="light gray", border="dark grey",lty=2)
    lines(start_day:end_day,NPP.AVG.mn,lty=1,col="black")
    points(start_day:end_day,NPP.AVG.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)
    #==========================================================================================================
    # Plant Resp
    #==========================================================================================================
    ylim = range(c(PLANT.RESP.AVG.ll,PLANT.RESP.AVG.ul),na.rm=TRUE) # define Y lims
    plot(start_day:end_day,PLANT.RESP.AVG.mn,xlab='',ylim=ylim,
         ylab=expression(paste(italic(R)[a]," (gC",~m^{-2},")")),pch=21,col="black", 
         bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx, rev(polyx)), c(PLANT.RESP.AVG.ul, rev(PLANT.RESP.AVG.ll)), 
            col="light gray", border="dark grey",lty=2)
    lines(start_day:end_day,PLANT.RESP.AVG.mn,lty=1,col="black")
    points(start_day:end_day,PLANT.RESP.AVG.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)
    #==========================================================================================================
    # Heterotrophic Resp
    #==========================================================================================================
    ylim = range(c(HTROPH.RESP.AVG.ll,HTROPH.RESP.AVG.ul),na.rm=TRUE) # define Y lims
    plot(start_day:end_day,HTROPH.RESP.AVG.mn,xlab='',ylim=ylim,
         ylab=expression(paste(italic(R)[h]," (gC",~m^{-2},")")),pch=21,col="black", 
         bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx, rev(polyx)), c(HTROPH.RESP.AVG.ul, rev(HTROPH.RESP.AVG.ll)), 
            col="light gray", border="dark grey",lty=2)
    lines(start_day:end_day,HTROPH.RESP.AVG.mn,lty=1,col="black")
    points(start_day:end_day,HTROPH.RESP.AVG.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)
    #==========================================================================================================
    # Reco
    #==========================================================================================================
    ylim = range(c(Reco.AVG.ll,Reco.AVG.ul),na.rm=TRUE)
    plot(start_day:end_day,Reco.AVG.mn,xlab=paste("DOY",as.character(year)),ylim=ylim,
         ylab=expression(paste(italic(R)[eco.]," (gC",~m^{-2},")")),
         pch=21,col="black", bg="black",
         cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx, rev(polyx)), c(Reco.AVG.ul, rev(Reco.AVG.ll)),col="light gray", border="dark grey",lty=2)
    lines(start_day:end_day,Reco.AVG.mn,lty=1,col="black")
    points(start_day:end_day,Reco.AVG.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)
    #==========================================================================================================
    # NEE
    #==========================================================================================================
    ylim = range(c(NEE.AVG.ll,NEE.AVG.ul),na.rm=TRUE) 
    plot(start_day:end_day,NEE.AVG.mn,xlab=paste("DOY",as.character(year)),ylim=ylim,
         ylab=expression(paste(NEE," (gC",~m^{-2},")")),
         pch=21,col="black", bg="black",
         cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx, rev(polyx)), c(NEE.AVG.ul, rev(NEE.AVG.ll)),col="light gray", border="dark grey",lty=2)
    lines(start_day:end_day,NEE.AVG.mn,lty=1,col="black")
    points(start_day:end_day,NEE.AVG.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")    
    box(lwd=2.2)
    
    # add single title to plot
    mtext("Site Component Fluxes", side=3, line=1, outer=TRUE, cex=1.5, font=2)
    
    ######################################## RESPIRATION COMPONENTS ###########################################
    par(mfrow=c(3,2),mar=c(5,5.7,0.9,0.5),mgp=c(3.3,1.5,0),oma=c(0,0,3,0)) # B, L, T, R
    #==========================================================================================================
    # Plant resp
    #==========================================================================================================
    ylim = range(c(PLANT.RESP.AVG.ll,PLANT.RESP.AVG.ul),na.rm=TRUE) # define Y lims
    plot(start_day:end_day,PLANT.RESP.AVG.mn,xlab='',ylim=ylim,
         ylab=expression(paste(italic(R)[a]," (gC",~m^{-2},")")),pch=21,col="black",
         bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx, rev(polyx)), c(PLANT.RESP.AVG.ul, rev(PLANT.RESP.AVG.ll)),
            col="light gray", border="dark grey",lty=2)
    lines(start_day:end_day,PLANT.RESP.AVG.mn,lty=1,col="black")
    points(start_day:end_day,PLANT.RESP.AVG.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2) 
    #==========================================================================================================
    # Leaf resp
    #==========================================================================================================
    ylim = range(c(LEAF.RESP.AVG.ll,LEAF.RESP.AVG.ul),na.rm=TRUE)
    plot(start_day:end_day,LEAF.RESP.AVG.mn,xlab='',ylim=ylim,
         ylab=expression(paste(italic(R)[leaf]," (gC",~m^{-2},")")),pch=21,col="black", 
          bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx,rev(polyx)),c(LEAF.RESP.AVG.ul,rev(LEAF.RESP.AVG.ll)),col="light gray",
            border="dark grey",lty=2)
    lines(start_day:end_day,LEAF.RESP.AVG.mn,lty=1,col="black")
    points(start_day:end_day,LEAF.RESP.AVG.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)    
    #==========================================================================================================
    # Root resp
    #==========================================================================================================
    ylim = range(c(ROOT.RESP.AVG.ll,ROOT.RESP.AVG.ul),na.rm=TRUE)
    plot(start_day:end_day,ROOT.RESP.AVG.mn,xlab='',ylim=ylim,
         ylab=expression(paste(italic(R)[root]," (gC",~m^{-2},")")),pch=21,col="black", 
         bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx,rev(polyx)),c(ROOT.RESP.AVG.ul,rev(ROOT.RESP.AVG.ll)),col="light gray",
            border="dark grey",lty=2)
    lines(start_day:end_day,ROOT.RESP.AVG.mn,lty=1,col="black")
    points(start_day:end_day,ROOT.RESP.AVG.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)
    #==========================================================================================================
    # Growth Resp
    #==========================================================================================================
    ylim = range(c(GROWTH.RESP.AVG.ll,GROWTH.RESP.AVG.ul),na.rm=TRUE)
    plot(start_day:end_day,GROWTH.RESP.AVG.mn,xlab='',ylim=ylim,
         ylab=expression(paste(italic(R)[growth]," (gC",~m^{-2},")")),pch=21,col="black", 
         bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx,rev(polyx)),c(GROWTH.RESP.AVG.ul,rev(GROWTH.RESP.AVG.ll)),col="light gray",
            border="dark grey",lty=2)
    lines(start_day:end_day,GROWTH.RESP.AVG.mn,lty=1,col="black")
    points(start_day:end_day,GROWTH.RESP.AVG.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)
    #==========================================================================================================
    # Storage Resp
    #==========================================================================================================
    ylim = range(c(STORAGE.RESP.AVG.ll,STORAGE.RESP.AVG.ul),na.rm=TRUE)
    plot(start_day:end_day,STORAGE.RESP.AVG.mn,xlab='',ylim=ylim,
         ylab=expression(paste(italic(R)[growth]," (gC",~m^{-2},")")),pch=21,col="black", 
         bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx,rev(polyx)),c(STORAGE.RESP.AVG.ul,rev(STORAGE.RESP.AVG.ll)),col="light gray",
            border="dark grey",lty=2)
    lines(start_day:end_day,STORAGE.RESP.AVG.mn,lty=1,col="black")
    points(start_day:end_day,STORAGE.RESP.AVG.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)
    #==========================================================================================================
    # Vleaf resp
    #==========================================================================================================    
    ylim = range(c(VLEAF.RESP.AVG.ll,VLEAF.RESP.AVG.ul),na.rm=TRUE)
    plot(start_day:end_day,VLEAF.RESP.AVG.mn,xlab='',ylim=ylim,
         ylab=expression(paste(italic(VR)[leaf]," (gC",~m^{-2},")")),pch=21,col="black", 
         bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx,rev(polyx)),c(VLEAF.RESP.AVG.ul,rev(VLEAF.RESP.AVG.ll)),col="light gray",
            border="dark grey",lty=2)
    lines(start_day:end_day,VLEAF.RESP.AVG.mn,lty=1,col="black")
    points(start_day:end_day,VLEAF.RESP.AVG.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)
    
    # Plot title
    mtext("Site Component Respiration ", side=3, line=1, outer=TRUE, cex=1.5, font=2)
    
    ########################################### Energy Balance ################################################
    par(mfrow=c(3,2),mar=c(5,5.7,0.9,0.5),mgp=c(3.3,1.5,0),oma=c(0,0,3,0)) # B, L, T, R
    
    #==========================================================================================================
    # Polygon vegetation/leaf vapor flux
    #==========================================================================================================
    ylim = range(c(AVG.VAPOR.LC.ll,AVG.VAPOR.LC.ul),na.rm=TRUE)
    plot(start_day:end_day,AVG.VAPOR.LC.mn,xlab='',ylim=ylim,
          ylab=expression(paste(V.~Flux[veg~to~CAS]," (kg",~m^{-2}~s^{-1},")")),pch=21,col="black", 
          bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx,rev(polyx)),c(AVG.VAPOR.LC.ul,rev(AVG.VAPOR.LC.ll)),col="light gray",
            border="dark grey",lty=2)
    lines(start_day:end_day,AVG.VAPOR.LC.mn,lty=1,col="black")
    points(start_day:end_day,AVG.VAPOR.LC.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)
    #==========================================================================================================
    # Polygon moisture flux ground to canopy air
    #==========================================================================================================                            
    ylim = range(c(AVG.VAPOR.GC.ll,AVG.VAPOR.GC.ul),na.rm=TRUE)
    plot(start_day:end_day,AVG.VAPOR.GC.mn,xlab='',ylim=ylim,
         ylab=expression(paste(V.~Flux[ground~to~CAS]," (kg",~m^{-2}~s^{-1},")")),pch=21,col="black", 
         bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx,rev(polyx)),c(AVG.VAPOR.GC.ll,rev(AVG.VAPOR.GC.ul)),col="light gray",
            border="dark grey",lty=2)
    lines(start_day:end_day,AVG.VAPOR.GC.mn,lty=1,col="black")
    points(start_day:end_day,AVG.VAPOR.GC.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)                            
    #==========================================================================================================
    # Polygon vapor flux atmosphere to canopy air
    #==========================================================================================================                          
    ylim = range(c(AVG.VAPOR.AC.ll,AVG.VAPOR.AC.ul),na.rm=TRUE)
    plot(start_day:end_day,AVG.VAPOR.AC.mn,xlab='',ylim=ylim,
         ylab=expression(paste(V.~Flux[atm.~to~CAS]," (kg",~m^{-2}~s^{-1},")")),pch=21,col="black", 
         bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx,rev(polyx)),c(AVG.VAPOR.AC.ul,rev(AVG.VAPOR.AC.ll)),col="light gray",
            border="dark grey",lty=2)
    lines(start_day:end_day,AVG.VAPOR.AC.mn,lty=1,col="black")
    points(start_day:end_day,AVG.VAPOR.AC.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)                         
    #==========================================================================================================
    # Polygon transpiration from stomata to canopy air spac
    #==========================================================================================================
    ylim = range(c(AVG.TRANSP.ll,AVG.TRANSP.ul),na.rm=TRUE)
    plot(start_day:end_day,AVG.TRANSP.mn,xlab='',ylim=ylim,
         ylab=expression(paste(Transpiration," (kg",~m^{-2}~s^{-1},")")),pch=21,col="black", 
         bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx,rev(polyx)),c(AVG.TRANSP.ul,rev(AVG.TRANSP.ll)),col="light gray",
            border="dark grey",lty=2)
    lines(start_day:end_day,AVG.TRANSP.mn,lty=1,col="black")
    points(start_day:end_day,AVG.TRANSP.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)
    #==========================================================================================================
    # Polygon averaged evap/dew from ground and leaves to C
    #==========================================================================================================
    ylim = range(c(AVG.EVAP.ll,AVG.EVAP.ul),na.rm=TRUE)
    plot(start_day:end_day,AVG.EVAP.mn,xlab='',ylim=ylim,
         ylab=expression(paste(Evaporation," (kg",~m^{-2}~s^{-1},")")),pch=21,col="black", 
         bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx,rev(polyx)),c(AVG.EVAP.ul,rev(AVG.EVAP.ll)),col="light gray",
            border="dark grey",lty=2)
    lines(start_day:end_day,AVG.EVAP.mn,lty=1,col="black")
    points(start_day:end_day,AVG.EVAP.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)
    #==========================================================================================================
    # Polygon Average Specific Humidity of Canopy Air
    #==========================================================================================================
    ylim = range(c(AVG.CAN.SHV.ll,AVG.CAN.SHV.ul),na.rm=TRUE)
    plot(start_day:end_day,AVG.CAN.SHV.mn,xlab='',ylim=ylim,
         ylab=expression(paste(Sp.Humidity[CAS]," (kg",~kg^{-1},")")),pch=21,col="black", 
         bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx,rev(polyx)),c(AVG.CAN.SHV.ul,rev(AVG.CAN.SHV.ll)),col="light gray",
            border="dark grey",lty=2)
    lines(start_day:end_day,AVG.CAN.SHV.mn,lty=1,col="black")
    points(start_day:end_day,AVG.CAN.SHV.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)
    #==========================================================================================================
    # Polygon wood to canopy air vapor flux
    #==========================================================================================================
#     ylim = range(c(AVG.VAPOR.WC.ll,AVG.VAPOR.WC.ul),na.rm=TRUE)
#     plot(start_day:end_day,AVG.VAPOR.WC.mn,xlab='',ylim=ylim,
#          ylab=expression(paste(italic(Vapor Flux)[wood]," (kg",~m^{-2},~s^{-1}")")),pch=21,col="black", 
#          bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
#     polygon(c(polyx,rev(polyx)),c(AVG.VAPOR.WC.ul,rev(AVG.VAPOR.WC.ll)),col="light gray",
#             border="dark grey",lty=2)
#     lines(start_day:end_day,AVG.VAPOR.WC.mn,lty=1,col="black")
#     points(start_day:end_day,AVG.VAPOR.WC.mn,pch=21,col="black", bg="black",
#            cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
#     abline(h=0,lty=2,lwd=1.5,col="black")
#     box(lwd=2.2)
    # Plot title
    mtext("Site Vapor Fluxes ", side=3, line=1, outer=TRUE, cex=1.5, font=2)
    ##################################### MET ##########################################
    
    #plot(start_day:end_day,AVG.SOIL.TEMP.5cm)

    #plot(start_day:end_day,AVG.SOIL.TEMP.10cm)
    
    #mtext("Site Soil Temperatures ", side=3, line=1, outer=TRUE, cex=1.5, font=2)
    
    dev.off() # Close PDF

  } # END for loop
}
#----------------------------------------------------------------------------------------------------#


#----------------------------------------------------------------------------------------------------#
# Plot monthly
plot_monthly = function(model.run,in.dir,out.dir){
  # UNDER DEVELOPMENT
  #--------------------------------------------------------------------------------------------------#
  when            = NULL
  pft.names       = c("C4 Grass","Early Tropical","Mid Tropical","Late Tropical"
                      ,"C3 Grass","North Pine","South Pine","Late Conifer"
                      ,"Early Temperate","Mid Temperate","Late Temperate"
                      ,"C3 Pasture","C3 Crop","C4 Pasture","C4 Crop","Subtropical C3 grass ",
                      "Araucaria","Total")
  n.pft           = length(pft.names) - 1
  #--------------------------------------------------------------------------------------------------#
  
  
  #--------------------------------------------------------------------------------------------------#

  #--------------------------------------------------------------------------------------------------#
  
  
  #----------------------------------------------------------------------------------------------#
  #       Loop over time.                                                                        #
  #----------------------------------------------------------------------------------------------#
  i = 1 # counter printing variable names to log file

  for (year in start_year:end_year) {
    message(paste("--- PROCESSING YEAR: ",year," ---"))
    
    
    #--------------------------------------------------------------------------------------------#
    if (year == start_year){
      month.begin = IMONTHA
    }else{
      month.begin = 1
    } #end if
    
    if (year == end_year){
      month.end = IMONTHZ
    }else{
      month.end = 12
    } #end if
    
    #n.months   = (as.numeric(month.end)-as.numeric(month.begin))+1
    
    n.months     = -12+as.numeric(month.end)+(12-as.numeric(month.begin)+1)
    nplant.pft = matrix(0,nrow=n.months,ncol=n.pft+1)
    lai.pft    = matrix(0,nrow=n.months,ncol=n.pft+1)
    agb.pft    = matrix(0,nrow=n.months,ncol=n.pft+1)
    coh.area   = list()
    coh.age    = list()
    coh.dbh    = list()
    coh.pft    = list()
    coh.nplant = list()
    coh.height = list()
    coh.gpp    = list()
    coh.resp   = list()
    coh.npp    = list()
    #--------------------------------------------------------------------------------------------#
    
    j = 0 # counter for month in output
    for (mm in month.begin:month.end) {
      j = j+1

      mth = toupper(mon2mmm(mm,lang="English")) #<--- convert month num to 3 letter name
      message(paste("-------- PROCESSING MONTH: ",mth))
      
      when.now  = chron(dates=paste(mm,1,year,sep="/"),times=paste(0,0,0,sep=":"))
      when      = c(when,when.now)
      
      #---------------- Load ED2 Model Output (hdf5) ----------------------------------------------#
      filename = list.files(in.dir,full.names=TRUE,
                          pattern=paste('.*-E-', year, '-.*.h5', sep=''))[1]
      if (is.na(filename)==1) {
        break
      }else{
        data <- hdf5load(filename, load = FALSE,tidy=TRUE) # LOAD ED2 OUTPUT
      }
      var_names = summary(data) # View info about vars. For debugging
      if (i==1){
        print("Mean Monthly Output Variables (IMOUTPUT)")
        print(var_names)
        print("")
      } # end of complex if/then
      #--------------------------------------------------------------------------------------------#
      
      
      #------------------------------------------------------------------------------------#
      #     Get desired PFT-level variables                                                #
      #------------------------------------------------------------------------------------#
      lai.pft  [j,1:n.pft] = data$MMEAN.LAI.PFT
      message(data.frame(data$MMEAN.LAI.PFT))
      agb.pft  [j,1:n.pft] = data$AGB.PFT
      #------------------------------------------------------------------------------------#
      
      
      #------------------------------------------------------------------------------------#
      #        Define the global number of patches and cohorts.                            #
      #------------------------------------------------------------------------------------#
      npatches.global = data$NPATCHES.GLOBAL
      ncohorts.global = data$NCOHORTS.GLOBAL
      #----- Find the indices for the beginning and end of each patch. --------------------#
      ncohorts = diff(c(data$PACO.ID,ncohorts.global+1))
      aco      = data$PACO.ID
      zco      = data$PACO.ID + ncohorts - 1
      #------------------------------------------------------------------------------------#
      
      
      #------------------------------------------------------------------------------------#
      #     Extend the area and age of each patch so it has the same length as the         #
      # cohorts.                                                                           #
      #------------------------------------------------------------------------------------#
      coh.area[[j]] = rep(data$AREA,times=ncohorts)
      coh.age [[j]] = rep(data$AGE ,times=ncohorts)
      #------------------------------------------------------------------------------------#
      
      
      #----- Grab other cohort-level variables. -------------------------------------------#
      coh.pft    [[j]] = data$PFT
      message(data$PFT)
      coh.dbh    [[j]] = data$DBH
      coh.nplant [[j]] = data$NPLANT*coh.area[[j]]
      coh.height [[j]] = data$HITE
      coh.gpp    [[j]] = data$MMEAN.GPP.CO
      coh.resp   [[j]] = ( data$MMEAN.LEAF.RESP.CO 
                           + data$MMEAN.ROOT.RESP.CO 
                           + data$MMEAN.GROWTH.RESP.CO 
                           + data$MMEAN.STORAGE.RESP.CO 
                           + data$MMEAN.VLEAF.RESP.CO   )
      coh.npp    [[j]] = coh.gpp[[j]] - coh.resp[[j]]  # NPP
      #------------------------------------------------------------------------------------#

       
      i=i+1 # counter for printing variable names to log file
      
    } # end for loop for importing monthly data for year x
    
    
    #------------------------------------------------------------------------------------------#
    #      Find which PFTs we use, and set any NA to zero (in case a PFT goes extinct).        #
    #------------------------------------------------------------------------------------------#
    tot                 = n.pft + 1  # <---- total cohort
    agb.pft      [,tot] = rowSums(agb.pft      [,1:n.pft])
    lai.pft      [,tot] = rowSums(lai.pft      [,1:n.pft])
    #message(lai.pft)
    #lai.pft
    pft.use             = which(colSums(agb.pft) > 0)
    #------------------------------------------------------------------------------------------#
    
    
    #==========================================================================================#
    #  Figures                                                                                 #
    #==========================================================================================#
    #     Plot the LAI of all PFTs together.                                                   #
    #------------------------------------------------------------------------------------------#
    pdf(paste(out.dir,"/","ED2_",year,"_Monthly_Mean_Output.pdf",sep=""),width=10,height=10,
        onefile=TRUE)
    
    #----- Find the limits and expand the range so the legend fits. ---------------------------#
    lai.ylim    = range(lai.pft,na.rm=TRUE)
    lai.ylim[2] = lai.ylim[2] + 0.2 * (lai.ylim[2] - lai.ylim[1])
    lai.title   = paste("Leaf Area Index","US-WCr",sep=" - ") # <--- Site needs to be dynamic
    lai.xlab    = "Month"
    lai.ylab    = expression(paste("LAI (",m^{2}~m^{-2},")"))   #"LAI [m2/m2]"
    
    plot(x=when,y=lai.pft[,1],type="n",ylim=lai.ylim,xaxt="n"
         ,main=lai.title,xlab=lai.xlab,ylab=lai.ylab)
    
    dev.off()
    
    
  } # end for loop 
  
  

  
} # end of function
#----------------------------------------------------------------------------------------------------#
