######################################################################################################
# Plot functions for PEcAn ED2 Diagnostics
# 
# v1
#
#
#
######################################################################################################

# Plot Mean Daily
plot_daily = function(model.run,in.dir,out.dir){
  
}

# Plot Site Average Fluxes
site_fluxes = function(model.run,in.dir,out.dir){
  
  #---------------- Init. Arrays --------------------------------------------------------------------#
  GPP.AVG          = rep(0,times=n.range)
  VLEAF.RESP.AVG		= rep(0,times=n.range)
  LEAF.RESP.AVG 		= rep(0,times=n.range) 
  STORAGE.RESP.AVG	= rep(0,times=n.range)
  GROWTH.RESP.AVG		= rep(0,times=n.range)
  ROOT.RESP.AVG		= rep(0,times=n.range)
  PLANT.RESP.AVG   	= rep(0,times=n.range)
  HTROPH.RESP.AVG		= rep(0,times=n.range)
  Reco.AVG		= rep(0,times=n.range)
  NPP.AVG			= rep(0,times=n.range)
  NEE.AVG      		= rep(0,times=n.range)
  SOIL.TEMP.AVG		= rep(0,times=n.range)
  CAN.AIR.TEMP.AVG	= rep(0,times=n.range)
  SWC.AVG			= rep(0,times=n.range)
  
  #---------------- Pheno data, if exists ----------------------------------------------------------#
  # probably should get rid of this as it is mostly ED specific
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
  
  i = 1
  for (year in start_year:end_year) {
    message(paste("--- PROCESSING YEAR: ",year," ---"))
    
    #---------------- Generate Subset Length --------------------------------------------------#
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
    
    initial=24   # Midday ---> NEED TO MAKE THIS DYNAMIC
    subset = seq(initial, (out_day*(1+as.numeric(end_day)-as.numeric(start_day))), 
                 out_day)
    
    #---------------- Load ED2 Model Output (hdf5) --------------------------------------------#
    filename = list.files(in.dir,full.names=TRUE,
                          pattern=paste('.*-T-', year, '-.*.h5', sep=''))[1]
    if (is.na(filename)==1) {
      break  # BREAK OUT OF LOOP IF LAST YEAR DOESN'T CONTAIN DATA
    }else{
      data <- hdf5load(filename, load = FALSE,tidy=TRUE) # LOAD ED2 OUTPUT
    }
    var_names = summary(data) # View info about vars. For debugging
    if (i==1){
      print("Site Averaged Fluxes (ITOUTPUT)")
      print(var_names)
      print("")
    }
    
    
    #---------------- Get Phenology Information -----------------------------------------------#
    chk = which(site_pheno==year)
    if (is.nan(mean(chk))==1) {
      phenology = data.frame(-9999,-9999,-9999)
      names(phenology)=c("Year","Greenup","LeafOff")
      GS_LENGTH = NA
    }else{
      phenology = site_pheno[chk,]
      GS_LENGTH = phenology[,3]-phenology[,2]
    }
    #------------------------------------------------------------------------------------------#
    
    
    #---------------- Generate Figures --------------------------------------------------------#
    ######################## SETUP PLOT PARAMETERS #############################################
    cex=2.6
    labcex = 2
    axiscex = 2
    maincex = 2
    umol2gc <- 1.0368 # conver to gC
    ######################## ED2 OUTPUT ########################################################
    GPP.AVG       	= data$AVG.GPP[subset]
    LEAF.RESP.AVG		= data$AVG.LEAF.RESP[subset]
    VLEAF.RESP.AVG		= data$AVG.VLEAF.RESP[subset]
    STORAGE.RESP.AVG	= data$AVG.STORAGE.RESP[subset]
    GROWTH.RESP.AVG		= data$AVG.GROWTH.RESP[subset]
    ROOT.RESP.AVG		= data$AVG.ROOT.RESP[subset]
    PLANT.RESP.AVG  	= data$AVG.PLANT.RESP[subset] ## Extract "tower" plant resp
    HTROPH.RESP.AVG 	= data$AVG.HTROPH.RESP[subset]
    Reco.AVG		= (PLANT.RESP.AVG + HTROPH.RESP.AVG) * umol2gc 
    NPP.AVG        		= (GPP.AVG - PLANT.RESP.AVG)  * umol2gc
    NEE.AVG         	= (GPP.AVG - (PLANT.RESP.AVG + HTROPH.RESP.AVG))  * umol2gc
    SOIL.TEMP.AVG		= (data$AVG.SOIL.TEMP[subset,1,9])-273.15 # convert to celcius
    CAN.AIR.TEMP.AVG	= (data$AVG.CAN.TEMP[subset])-273.15 # convert to celcius
    SWC.AVG			= data$AVG.SOIL.WATER[subset,1,9] # soil moisture at 5cm
    ############################################################################################
    pdf(paste(out.dir,"/","ED2_",year,"_Site_Avg_Fluxes.pdf",sep=""),width=12,height=11,
        onefile=TRUE)
    par(mfrow=c(3,2),mar=c(5,5.4,0.6,0.2),mgp=c(3.3,1.5,0)) # B, L, T, R
    
    ######################### COMPONENT FLUXES ###########################
    ## GPP
    plot(start_day:end_day,GPP.AVG,xlab='',ylab=expression(paste(GPP," (gC",~m^{-2}~d^{-1},")")),
         pch=21,col="dark grey", bg="dark grey",
         cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    abline(v=phenology[,2],lty=2,lwd=1.5,col="green3")
    abline(v=phenology[,3],lty=2,lwd=1.5,col="brown")
    lines(smooth.spline(start_day:end_day,GPP.AVG,spar=0.4),col="black",lwd=5)
    if (is.nan(mean(chk))==0) {
      legend("topleft",legend=c("Greenup","Leaf Off"),bty="n",
             lty=2,lwd=1.5,col=c("green3","brown"),cex=2)
      #text(37,max(GPP.AVG)-4,"GS Length:",cex=2)
      #text(35,max(GPP.AVG)-5,paste(round(GS_LENGTH,2)," days",sep=""),
      #    cex=2 )
    }
    rm(chk)
    box(lwd=2.2)
    
    ## NPP
    plot(start_day:end_day,NPP.AVG,xlab='',ylab=expression(paste(NPP," (gC",~m^{-2}~d^{-1},")")),
         pch=21,col="dark grey", bg="dark grey",
         cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    abline(v=phenology[,2],lty=2,lwd=1.5,col="green3")
    abline(v=phenology[,3],lty=2,lwd=1.5,col="brown")
    lines(smooth.spline(start_day:end_day,NPP.AVG,spar=0.4),col="black",lwd=5)
    box(lwd=2.2)
    
    ## Plant Resp
    plot(start_day:end_day,PLANT.RESP.AVG*umol2gc,xlab='',
         ylab=expression(paste(italic(R)[a]," (gC",~m^{-2}~d^{-1},")")),pch=21,col="dark grey", 
         bg="dark grey",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    lines(smooth.spline(start_day:end_day,PLANT.RESP.AVG*umol2gc,spar=0.4),col="black",lwd=5)
    box(lwd=2.2)
    
    ## Heterotrophic Resp
    plot(start_day:end_day,HTROPH.RESP.AVG*umol2gc,xlab='',
         ylab=expression(paste(italic(R)[h]," (gC",~m^{-2}~d^{-1},")")),pch=21,col="dark grey", 
         bg="dark grey",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    lines(smooth.spline(start_day:end_day,HTROPH.RESP.AVG*umol2gc,spar=0.4),col="black",lwd=5)
    box(lwd=2.2)
    
    ## Reco
    plot(start_day:end_day,Reco.AVG,xlab=paste("DOY",as.character(year)),
         ylab=expression(paste(italic(R)[eco.]," (gC",~m^{-2}~d^{-1},")")),
         pch=21,col="dark grey", bg="dark grey",
         cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    lines(smooth.spline(start_day:end_day,Reco.AVG,spar=0.4),col="black",lwd=5)
    box(lwd=2.2)
    
    ## NEE
    plot(start_day:end_day,NEE.AVG,xlab=paste("DOY",as.character(year)),
         ylab=expression(paste(NEE," (gC",~m^{-2}~d^{-1},")")),
         pch=21,col="dark grey", bg="dark grey",
         cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    lines(smooth.spline(start_day:end_day,NEE.AVG,spar=0.4),col="black",lwd=5)
    box(lwd=2.2)
    #dev.off()
    
    
    ############################# RESPIRATION COMPONENTS ###############################
    par(mfrow=c(3,2),mar=c(5,5.6,0.6,0.2),mgp=c(3.3,1.5,0)) # B, L, T, R
    ## Plant Resp
    plot(start_day:end_day,PLANT.RESP.AVG*umol2gc,xlab='',
         ylab=expression(paste(italic(R)[a]," (gC",~m^{-2}~d^{-1},")")),pch=21,
         col="dark grey",bg="dark grey",cex=cex,cex.lab=labcex,cex.axis=axiscex,
         cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    lines(smooth.spline(start_day:end_day,PLANT.RESP.AVG,spar=0.4),col="black",lwd=5)
    box(lwd=2.2)
    
    ## Leaf Resp
    plot(start_day:end_day,LEAF.RESP.AVG*umol2gc,xlab='',
         ylab=expression(paste(italic(R)[leaf]," (gC",~m^{-2}~d^{-1},")")),pch=21,col="dark grey", 
         bg="dark grey",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    lines(smooth.spline(start_day:end_day,LEAF.RESP.AVG*umol2gc,spar=0.4),col="black",lwd=5)
    box(lwd=2.2)
    
    ## Root Resp
    plot(start_day:end_day,ROOT.RESP.AVG*umol2gc,xlab='',
         ylab=expression(paste(italic(R)[root]," (gC",~m^{-2}~d^{-1},")")),pch=21,col="dark grey", 
         bg="dark grey",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    lines(smooth.spline(start_day:end_day,ROOT.RESP.AVG*umol2gc,spar=0.4),col="black",lwd=5)
    box(lwd=2.2)
    
    ## Growth Resp
    plot(start_day:end_day,GROWTH.RESP.AVG*umol2gc,xlab='',
         ylab=expression(paste(italic(R)[growth]," (gC",~m^{-2}~d^{-1},")")),pch=21,col="dark grey", 
         bg="dark grey",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    lines(smooth.spline(start_day:end_day,GROWTH.RESP.AVG*umol2gc,spar=0.4),col="black",lwd=5)
    box(lwd=2.2)	
    
    # Storage Resp
    plot(start_day:end_day,STORAGE.RESP.AVG*umol2gc,xlab=paste("DOY",as.character(year)),
         ylab=expression(paste(italic(R)[storage]," (gC",~m^{-2}~d^{-1},")")),pch=21,col="dark grey", 
         bg="dark grey",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    lines(smooth.spline(start_day:end_day,STORAGE.RESP.AVG*umol2gc,spar=0.4),col="black",lwd=5)
    box(lwd=2.2)
    
    ## VLeaf Resp
    plot(start_day:end_day,VLEAF.RESP.AVG*umol2gc,xlab=paste("DOY",as.character(year)),
         ylab=expression(paste(italic(R)[Vleaf]," (gC",~m^{-2}~d^{-1},")")),pch=21,col="dark grey", 
         bg="dark grey",cex=cex,cex.lab=labcex,cex.axis=axiscex,cex.main=maincex)
    abline(h=0,lty=2,lwd=1.5,col="black")
    lines(smooth.spline(start_day:end_day,VLEAF.RESP.AVG*umol2gc,spar=0.4),col="black",lwd=5)
    
    ################################## Energy Balance #####################################
    
    
    
    ##################################### MET ##########################################
    
    dev.off() # Close PDF
    i=i+1
  } # END for loop
}