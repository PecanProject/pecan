## Set default fixed parameters below: 
pecan.config.constants <- function(pft) {
  if(pft == 'ebifarm.c4crop'){
    
    
    PFT <- xmlNode ("pft")
    PFT <- append.xmlNode(PFT, xmlNode("num", 15))
    PFT <- append.xmlNode(PFT, xmlNode("max_dbh", 0.78))
    PFT <- append.xmlNode(PFT, xmlNode("hgt_min", 2.0))
    PFT <- append.xmlNode(PFT, xmlNode("dark_respiration_factor",0.015 ))
    PFT <- append.xmlNode(PFT, xmlNode("qsw", 0.0))
    PFT <- append.xmlNode(PFT, xmlNode("mort1", 1.0))
    PFT <- append.xmlNode(PFT, xmlNode("plant_min_temp", -100))
                                        #  PFT <- append.xmlNode(PFT, xmlNode("quantum_efficiency", 0.08))
    PFT <- append.xmlNode(PFT, xmlNode("storage_turnover_rate", 0))

                                        #  RAD <- xmlNode("radiation")
                                        #  RAD <- append.xmlNode(RAD, xmlNode("lai_min", 0.0001))

    MISC <- xmlNode("ed_misc")
    MISC <- append.xmlNode(MISC, xmlNode("outputMonth", 11))

    CONFIG <- xmlNode("config")
                                        #  CONFIG <- append.xmlNode(CONFIG, RAD)
    CONFIG <- append.xmlNode(CONFIG, MISC) 
    
  }

  if(pft == 'poplar.tempdecid') {
    
    HYDRO <- xmlNode("hydro")
    HYDRO <- append.xmlNode(HYDRO, xmlNode("useTOPMODEL", 0))
    HYDRO <- append.xmlNode(HYDRO, xmlNode("useRUNOFF", 0))  

    MISC <- xmlNode("ed_misc")
    MISC <- append.xmlNode(MISC, xmlNode("burnin", 1))

    CONFIG <- xmlNode("config")
    CONFIG <- append.xmlNode(CONFIG, HYDRO)
    CONFIG <- append.xmlNode(CONFIG, MISC) 

    PFT <- xmlNode ("pft")
    PFT <- append.xmlNode(PFT, xmlNode("num", 9))
    PFT <- append.xmlNode(PFT, xmlNode("max_dbh", 75))
    PFT <- append.xmlNode(PFT, xmlNode("hgt_max", 25))
    PFT <- append.xmlNode(PFT, xmlNode("dark_respiration_factor",0.015 ))
    PFT <- append.xmlNode(PFT, xmlNode("growth_respiration_factor", 0))
    PFT <- append.xmlNode(PFT, xmlNode("leaf_turnover_rate", 1.0))
    PFT <- append.xmlNode(PFT, xmlNode("plant_min_temp", -80))


  }
  return(list(CONFIG = CONFIG, PFT = PFT))

}
