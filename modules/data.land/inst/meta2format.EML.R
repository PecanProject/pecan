library(devtools)
devtools::install_github("ropensci/EML") #install EML package
library(EML)

#Steps for meta2format.EML:
# 1) Extract the required metadata from the file
# 2) Insert metadata into the database.
# Brown Dog will extract the required metadata and
# insertion into DB happens only in pecan.
# function has to handle formats table and formats_variables table

# outside of meta2format create a generic function for inserting
# database formats and formats variables into Bety

#meta2format.EML <- function(){}
###try on file I gave to Gene
#f3 <- "/Users/josh/Downloads/doi_10.5063_AA_nceas.980.2-METADATA(1).xml"
f3 <- "/fs/data3/jam2767/EML/doi_10.5063_AA_nceas.980.2-METADATA.xml"
eml3 <- read_eml(f3)
eml_validate(eml3)
dt3 <- eml_get(eml3, "dataTable")
length(dt3) #5 data tables
entities3 <- sapply(dt3, eml_get, "entityName")
urls3 <- sapply(dt3, eml_get, "url")
#urls3 <- sapply(dt3, function(x) x@physical[[1]]@distribution[[1]]@online@url)
attrs3 <- eml_get(dt3, "attributeName") #multiple ways to access attributes
length(attrs3)
#get_attributes(eml3@dataset@dataTable[[1]]@attributeList)
attr3 <- get_attributes(eml3@dataset@dataTable[[1]]@attributeList) #only getting attributes for 1st table
attr3$attributes$attributeName #look at attribute names
