group_to_type <- c(
  # woody
  "dates"                          = "woody",
  "avocados"                       = "woody",
  "olives"                         = "woody",
  "miscellaneous subtropical fruits" = "woody",
  "kiwis"                          = "woody",
  "apples"                         = "woody",
  "cherries"                       = "woody",
  "peaches and nectarines"         = "woody",
  "pears"                          = "woody",
  "miscellaneous deciduous"        = "woody",
  "mixed deciduous"                = "woody",
  "almonds"                        = "woody",
  "walnuts"                        = "woody",
  "pistachios"                     = "woody",
  "pomegranates"                   = "woody",
  "plums prunes or apricots"       = "woody",
  "bush berries"                   = "woody",
  "wine grapes"                    = "woody",
  
  # row crops
  "cotton"                         = "row",
  "safflower"                      = "row",
  "beans"                          = "row",
  "miscellaneous field"            = "row",
  "sunflowers"                     = "row",
  "corn sorghum sudan"             = "row",
  "wheat"                          = "row",
  "cole crops"                     = "row",
  "carrots"                        = "row",
  "melons squash and cucumber"     = "row",
  "onions and garlic"              = "row",
  "potatoes"                       = "row",
  "tomatoes"                       = "row",
  "flowers nursery and christmas tree farms" = "row",
  "miscellaneous truck"            = "row",
  "strawberries"                   = "row",
  "peppers"                        = "row",
  "greenhouse"                     = "row",
  "lettuce or leafy greens"        = "row",
  "potato or sweet potato"         = "row",
  
  # rice
  "rice"                           = "rice",
  "wild rice"                      = "rice",
  
  # hay
  "miscellaneous grain and hay"    = "hay"
)

map_group_to_type <- function(group_vec) {
  type <- group_to_type[group_vec]
  type[is.na(type)] <- "NA"
  return(unname(type))
}
