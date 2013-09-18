#!/usr/bin/Rscript --vanilla
args   <- commandArgs(trailingOnly = TRUE)
rundir <- args[1]
outdir <- args[2]

# set the libpath here to point to the right version of BioCro
require(XML)
require(BioCro)
require(PEcAn.utils)

# load the weather data
weather <- read.csv(file.path(rundir, "weather.csv"))
years <- unique(weather$year)

# run model
config <- xmlToList(xmlParse(file.path(rundir, "config.xml")))
pp.config <- config$pft$photoParms
pp <- lapply(photoParms(vmax=pp.config$vmax, b0=pp.config$b0, b1 = pp.config$b1,Rd=pp.config$Rd), as.numeric)
cc <- canopyParms(Sp = as.numeric(config$pft$canopyControl$Sp))

genus <- config$pft$genus
if(!(genus %in% c("Saccharum", "Salix", "Miscanthus"))) {
    logger.severe("genus", genus, "not supported by PEcAn.BIOCRO module")
}

result <- list()
for(yeari in years){
    yearchar <- as.character(yeari)
    WetDat <- weather[weather$year == yeari,]
    day1 <- min(WetDat$doy)
    dayn <- max(WetDat$doy)

    if(genus == "Saccharum"){
        result[[yearchar]] <- caneGro(WetDat = WetDat, photoControl=pp, canopyControl=cc)
        result[[yearchar]][["Grain"]] <- result[[yearchar]][["Rhizome"]] <- rep(0, length(result$Hour))
    } else if (genus == "Salix") {
        if(yeari == min(years)){
            iplant <- iwillowParms(iRhizome=1.0, iStem=1.0, iLeaf=0.0,
                                    iRoot=1.0, ifrRhizome=0.01, ifrStem=0.01,
                                    ifrLeaf = 0.0, ifrRoot = 0.0)
        } else if(yeari > min(years)){
            iplant$iRhizome <- last(result[[as.character(yeari-1)]]$Rhizome)
            iplant$iStem <- last(result[[as.character(yeari-1)]]$Stem)
            iplant$iRoot <- last(result[[as.character(yeari-1)]]$Root)
        }
        result[[yearchar]] <- willowGro(WetDat = WetDat, photoControl=pp,
                                        canopyControl=cc, day1 = day1, dayn = dayn)
    } else if (genus == "Miscanthus") {
        result[[yearchar]] <- BioGro(WetDat = WetDat, photoControl = pp, canopyControl = cc)
    }

    result.yeari <- with(result[[yearchar]],
    data.table(Year = yeari, DayofYear, Hour, ThermalT,
                Stem, Leaf, Root, Rhizome, Grain, LAI,
                SoilEvaporation, CanopyTrans))
    if(yeari == min(years)){
        resultDT <- result.yeari
    } else if (yeari > min(years)){
        resultDT <- rbind(resultDT, result.yeari)
    }
}

write.csv(resultDT, file=file.path(outdir, "result.csv"))
save(resultDT, config, file = file.path(outdir, "result.RData"))
