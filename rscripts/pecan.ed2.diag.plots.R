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
#  A work in progress
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
#----------------------------------------------------------------------------------------------------#

site_fluxes = function(model.run,in.dir,out.dir){
  
  #---------------- Init. Arrays --------------------------------------------------------------------#
  # Info: Initialize arrays for entire model run and populate with for loop (below)
  GPP.AVG           = rep(0,times=n.range)
  VLEAF.RESP.AVG		= rep(0,times=n.range)
  LEAF.RESP.AVG 		= rep(0,times=n.range) 
  STORAGE.RESP.AVG	= rep(0,times=n.range)
  GROWTH.RESP.AVG		= rep(0,times=n.range)
  ROOT.RESP.AVG		  = rep(0,times=n.range)
  PLANT.RESP.AVG   	= rep(0,times=n.range)
  HTROPH.RESP.AVG		= rep(0,times=n.range)
  Reco.AVG		      = rep(0,times=n.range)
  NPP.AVG			      = rep(0,times=n.range)
  NEE.AVG      		  = rep(0,times=n.range)
  SOIL.TEMP.AVG		  = rep(0,times=n.range)
  CAN.AIR.TEMP.AVG	= rep(0,times=n.range)
  SWC.AVG			      = rep(0,times=n.range)
  #--------------------------------------------------------------------------------------------------#
  
  #---------------- Pheno data, if exists -----------------------------------------------------------#
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
    vals_day    = out_day # <--- values written out per day, 86400/FRQFAST
    hdflength   = (vals_day*(1+end_day-start_day)) # length
    
    # Info from driver script
    # dates contains YYmmdd, month (num), doy. fjday (0-1)  
    init    = dates[1,4]
    total   = seq(1,hdflength,1)
    reps    = hdflength/vals_day
    dayfrac = rep(seq(0,23.5,0.5), each=1, times=reps) # t
    subset    = 0
    
    s      = seq(9.0,19.0,0.5) #<--- need to make this dynamic. user specific
    subset = which(dayfrac >= 9.0 & dayfrac <= 19.0) # just temporary.  need to make this dynamic
    hours = dayfrac[dayfrac >= 9.0 & dayfrac <= 19.0] 
    aggrlist = rep(start_day:(end_day), each=length(s))

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
      print("Site Averaged Fluxes (ITOUTPUT)")
      print(var_names) # Show variable names in log file
      print("")
    }
    i=i+1
    
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
    ######################## SETUP PLOT PARAMETERS ###################################################
    cex=0.8
    labcex = 2
    axiscex = 2
    maincex = 2
    umol2gc <- 1.0368 # conver to gC
    ######################## ED2 OUTPUT ##############################################################
    GPP.AVG       	        = data$AVG.GPP[subset]
    GPP.AVG.mn              = aggregate(GPP.AVG,by=list(aggrlist),mean)
    GPP.AVG.sd              = aggregate(GPP.AVG,by=list(aggrlist),sd)
    GPP.AVG.ll              = GPP.AVG.mn[,2] - GPP.AVG.sd[,2] 
    GPP.AVG.ul              = GPP.AVG.mn[,2] + GPP.AVG.sd[,2]
    #------------------------------------------------------------------------------------------#
    #LEAF.RESP.AVG		= data$AVG.LEAF.RESP[subset]
    #VLEAF.RESP.AVG		= data$AVG.VLEAF.RESP[subset]
    #STORAGE.RESP.AVG	= data$AVG.STORAGE.RESP[subset]
    #GROWTH.RESP.AVG		= data$AVG.GROWTH.RESP[subset]
    #ROOT.RESP.AVG		= data$AVG.ROOT.RESP[subset]
    #------------------------------------------------------------------------------------------#
    PLANT.RESP.AVG  	      = data$AVG.PLANT.RESP[subset] ## Extract "tower" plant resp
    PLANT.RESP.AVG.mn       = aggregate(PLANT.RESP.AVG,by=list(aggrlist),mean)
    PLANT.RESP.AVG.sd       = aggregate(PLANT.RESP.AVG,by=list(aggrlist),sd)
    PLANT.RESP.AVG.ll       = PLANT.RESP.AVG.mn[,2]-PLANT.RESP.AVG.sd[,2]
    PLANT.RESP.AVG.ul       = PLANT.RESP.AVG.mn[,2]+PLANT.RESP.AVG.sd[,2]
    #------------------------------------------------------------------------------------------#
    HTROPH.RESP.AVG 	      = data$AVG.HTROPH.RESP[subset]
    HTROPH.RESP.AVG.mn      = aggregate(HTROPH.RESP.AVG,by=list(aggrlist),mean)
    HTROPH.RESP.AVG.sd      = aggregate(HTROPH.RESP.AVG,by=list(aggrlist),sd)
    HTROPH.RESP.AVG.ll      = HTROPH.RESP.AVG.mn[,2]-HTROPH.RESP.AVG.sd[,2]
    HTROPH.RESP.AVG.ul      = HTROPH.RESP.AVG.mn[,2]+HTROPH.RESP.AVG.sd[,2]
    #------------------------------------------------------------------------------------------#
    Reco.AVG.mn	            = (PLANT.RESP.AVG.mn[,2] + HTROPH.RESP.AVG.mn[,2]) * umol2gc 
    Reco.AVG.ll             = (PLANT.RESP.AVG.ll + HTROPH.RESP.AVG.ll) * umol2gc
    Reco.AVG.ul             = (PLANT.RESP.AVG.ul + HTROPH.RESP.AVG.ul) * umol2gc
    #------------------------------------------------------------------------------------------#
    NPP.AVG.mn        		  = (GPP.AVG.mn[,2] - PLANT.RESP.AVG.mn[,2])  * umol2gc # avg
    NPP.AVG.ll              = (GPP.AVG.ll - PLANT.RESP.AVG.ll)  * umol2gc
    NPP.AVG.ul              = (GPP.AVG.ul - PLANT.RESP.AVG.ul)  * umol2gc
    #------------------------------------------------------------------------------------------#
    NEE.AVG.mn         	    = (GPP.AVG.mn[,2] - (PLANT.RESP.AVG.mn[,2] + HTROPH.RESP.AVG.mn[,2]))  * umol2gc
    NEE.AVG.ll              = (GPP.AVG.ll - (PLANT.RESP.AVG.ll + HTROPH.RESP.AVG.ll))  * umol2gc
    NEE.AVG.ul              = (GPP.AVG.ul - (PLANT.RESP.AVG.ul + HTROPH.RESP.AVG.ul))  * umol2gc
    #------------------------------------------------------------------------------------------#
    
    #SOIL.TEMP.AVG		= (data$AVG.SOIL.TEMP[subset,1,9])-273.15 # convert to celcius
    #CAN.AIR.TEMP.AVG	= (data$AVG.CAN.TEMP[subset])-273.15 # convert to celcius
    #SWC.AVG			= data$AVG.SOIL.WATER[subset,1,9] # soil moisture at 5cm
    
    ###########################################################################################################
    ##################################### COMPONENT FLUXES ####################################################
    pdf(paste(out.dir,"/","ED2_",year,"_Site_Avg_Fluxes.pdf",sep=""),width=12,height=11,
        onefile=TRUE)
    par(mfrow=c(3,2),mar=c(5,5.4,0.6,0.2),mgp=c(3.3,1.5,0)) # B, L, T, R
    
    #==========================================================================================================
    # GPP
    #==========================================================================================================
    ylim = range(c(GPP.AVG.ll,GPP.AVG.ul),na.rm=TRUE) # define Y lims
    plot(start_day:end_day,GPP.AVG.mn[,2],xlab='',ylab=expression(paste(GPP," (gC",~m^{-2}~d^{-1},")")),
         ylim=ylim,pch=21,col="black", bg="black",
         cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(v=phenology[,2],lty=2,lwd=1.5,col="green3")
    abline(v=phenology[,3],lty=2,lwd=1.5,col="brown")
    polygon(c(polyx, rev(polyx)), c(GPP.AVG.ul, rev(GPP.AVG.ll)), col="light gray", border="dark grey",lty=2)  
    lines(start_day:end_day,GPP.AVG.mn[,2],lty=1,col="black")
    points(start_day:end_day,GPP.AVG.mn[,2],pch=21,col="black", bg="black",
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
    plot(start_day:end_day,NPP.AVG.mn,xlab='',ylab=expression(paste(NPP," (gC",~m^{-2}~d^{-1},")")),
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
    plot(start_day:end_day,PLANT.RESP.AVG.mn[,2]*umol2gc,xlab='',ylim=ylim,
         ylab=expression(paste(italic(R)[a]," (gC",~m^{-2}~d^{-1},")")),pch=21,col="black", 
         bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx, rev(polyx)), c(PLANT.RESP.AVG.ul, rev(PLANT.RESP.AVG.ll)), 
            col="light gray", border="dark grey",lty=2)
    lines(start_day:end_day,PLANT.RESP.AVG.mn[,2],lty=1,col="black")
    points(start_day:end_day,PLANT.RESP.AVG.mn[,2],pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)
    
    #==========================================================================================================
    # Heterotrophic Resp
    #==========================================================================================================
    ylim = range(c(HTROPH.RESP.AVG.ll,HTROPH.RESP.AVG.ul),na.rm=TRUE) # define Y lims
    plot(start_day:end_day,HTROPH.RESP.AVG.mn[,2]*umol2gc,xlab='',ylim=ylim,
         ylab=expression(paste(italic(R)[h]," (gC",~m^{-2}~d^{-1},")")),pch=21,col="black", 
         bg="black",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx, rev(polyx)), c(HTROPH.RESP.AVG.ul, rev(HTROPH.RESP.AVG.ll)), 
            col="light gray", border="dark grey",lty=2)
    lines(start_day:end_day,HTROPH.RESP.AVG.mn[,2],lty=1,col="black")
    points(start_day:end_day,HTROPH.RESP.AVG.mn[,2],pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    box(lwd=2.2)
    
    #==========================================================================================================
    # Reco
    #==========================================================================================================
    ylim = range(c(Reco.AVG.ll,Reco.AVG.ul),na.rm=TRUE)
    plot(start_day:end_day,Reco.AVG.mn,xlab=paste("DOY",as.character(year)),ylim=ylim,
         ylab=expression(paste(italic(R)[eco.]," (gC",~m^{-2}~d^{-1},")")),
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
         ylab=expression(paste(NEE," (gC",~m^{-2}~d^{-1},")")),
         pch=21,col="black", bg="black",
         cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    polygon(c(polyx, rev(polyx)), c(NEE.AVG.ul, rev(NEE.AVG.ll)),col="light gray", border="dark grey",lty=2)
    lines(start_day:end_day,NEE.AVG.mn,lty=1,col="black")
    points(start_day:end_day,NEE.AVG.mn,pch=21,col="black", bg="black",
           cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")    
    box(lwd=2.2)
    
    ############################# RESPIRATION COMPONENTS ###############################
#     par(mfrow=c(3,2),mar=c(5,5.6,0.6,0.2),mgp=c(3.3,1.5,0)) # B, L, T, R
#     ## Plant Resp
#     plot(start_day:end_day,PLANT.RESP.AVG*umol2gc,xlab='',
#          ylab=expression(paste(italic(R)[a]," (gC",~m^{-2}~d^{-1},")")),pch=21,
#          col="dark grey",bg="dark grey",cex=cex,cex.lab=labcex,cex.axis=axiscex,
#          cex.main=maincex)
#     abline(h=0,lty=2,lwd=1.5,col="black")
#     lines(smooth.spline(start_day:end_day,PLANT.RESP.AVG,spar=0.4),col="black",lwd=5)
#     box(lwd=2.2)
#     
#     ## Leaf Resp
#     plot(start_day:end_day,LEAF.RESP.AVG*umol2gc,xlab='',
#          ylab=expression(paste(italic(R)[leaf]," (gC",~m^{-2}~d^{-1},")")),pch=21,col="dark grey", 
#          bg="dark grey",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
#     abline(h=0,lty=2,lwd=1.5,col="black")
#     lines(smooth.spline(start_day:end_day,LEAF.RESP.AVG*umol2gc,spar=0.4),col="black",lwd=5)
#     box(lwd=2.2)
#     
#     ## Root Resp
#     plot(start_day:end_day,ROOT.RESP.AVG*umol2gc,xlab='',
#          ylab=expression(paste(italic(R)[root]," (gC",~m^{-2}~d^{-1},")")),pch=21,col="dark grey", 
#          bg="dark grey",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
#     abline(h=0,lty=2,lwd=1.5,col="black")
#     lines(smooth.spline(start_day:end_day,ROOT.RESP.AVG*umol2gc,spar=0.4),col="black",lwd=5)
#     box(lwd=2.2)
#     
#     ## Growth Resp
#     plot(start_day:end_day,GROWTH.RESP.AVG*umol2gc,xlab='',
#          ylab=expression(paste(italic(R)[growth]," (gC",~m^{-2}~d^{-1},")")),pch=21,col="dark grey", 
#          bg="dark grey",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
#     abline(h=0,lty=2,lwd=1.5,col="black")
#     lines(smooth.spline(start_day:end_day,GROWTH.RESP.AVG*umol2gc,spar=0.4),col="black",lwd=5)
#     box(lwd=2.2)	
#     
#     # Storage Resp
#     plot(start_day:end_day,STORAGE.RESP.AVG*umol2gc,xlab=paste("DOY",as.character(year)),
#          ylab=expression(paste(italic(R)[storage]," (gC",~m^{-2}~d^{-1},")")),pch=21,col="dark grey", 
#          bg="dark grey",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
#     abline(h=0,lty=2,lwd=1.5,col="black")
#     lines(smooth.spline(start_day:end_day,STORAGE.RESP.AVG*umol2gc,spar=0.4),col="black",lwd=5)
#     box(lwd=2.2)
#     
#     ## VLeaf Resp
#     plot(start_day:end_day,VLEAF.RESP.AVG*umol2gc,xlab=paste("DOY",as.character(year)),
#          ylab=expression(paste(italic(R)[Vleaf]," (gC",~m^{-2}~d^{-1},")")),pch=21,col="dark grey", 
#          bg="dark grey",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
#     abline(h=0,lty=2,lwd=1.5,col="black")
#     lines(smooth.spline(start_day:end_day,VLEAF.RESP.AVG*umol2gc,spar=0.4),col="black",lwd=5)
#     
    ################################## Energy Balance #####################################
    
    
    
    ##################################### MET ##########################################
    
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
