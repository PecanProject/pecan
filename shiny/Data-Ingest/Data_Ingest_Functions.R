# resolve.id <- function(attributeTable, attribute, input, output) dplyr::filter(attributeTable, attribute == input) %>% pull(output)
# 
# resolve.id(attributeTable = sites_sub, attribute = sitename, input = sitename_test, output = id)
# 
# dplyr::filter(sites_sub, sitename == sitename_test) %>% pull(id)
