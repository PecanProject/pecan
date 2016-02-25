#install.packages("~/pecan/modules/rtm", repos=NULL)
library(PEcAnRTM)

trait.values <- list()
# trait.values[[1]] <- list(SLA = 1)
# trait.values[[2]] <- list(SLA = 2)
# names(trait.values) <- c("Optics.Temperate_Late_Conifer",
#                          'Optics.Temperate_Early_Hardwood')

xml <- write.config.xml.ED2(defaults = defaults,
                            settings = settings,
                            trait.values = trait.values)

print(xml)
