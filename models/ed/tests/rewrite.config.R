install.packages("~/pecan/models/ed", repos=NULL)
detach('package:PEcAn.ED2', unload=TRUE)
library(PEcAn.ED2)

# Define settings list
settings <- list()
settings$model$revision <- "git"
settings$model$config.header <- ''
settings$constants <- NULL

# Set test trait values
trait.values <- list(pft = list(name = "Optics.Temperate_Early_Hardwood",
                                orient_factor = 999),
                     pft = list(name = "Optics.Temperate_Mid_Hardwood",
                                mort1 = 999),
                     pft = list(name = "Optics.Temperate_Late_Hardwood",
                                leaf_reflect_vis = 999)
                     )

test.xml <- write.config.xml.ED2(settings, trait.values)
print(test.xml)
