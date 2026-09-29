library(readr)
library(dplyr)

# Create the mapping with proper structure
mapping <- tibble::tribble(
  ~CLASS, ~SUBCLASS, ~CLASS_desc, ~SUBCLASS_desc,
  "C", "4", "Citrus and subtropical", "dates",
  "C", "5", "Citrus and subtropical", "avocados",
  "C", "6", "Citrus and subtropical", "olives",
  "C", "7", "Citrus and subtropical", "miscellaneous subtropical fruits",
  "C", "8", "Citrus and subtropical", "kiwis",
  "D", "1", "Deciduous fruits and nuts", "apples",
  "D", "3", "Deciduous fruits and nuts", "cherries",
  "D", "5", "Deciduous fruits and nuts", "peaches and nectarines",
  "D", "6", "Deciduous fruits and nuts", "pears",
  "D", "10", "Deciduous fruits and nuts", "miscellaneous deciduous",
  "D", "11", "Deciduous fruits and nuts", "mixed deciduous",
  "D", "12", "Deciduous fruits and nuts", "almonds",
  "D", "13", "Deciduous fruits and nuts", "walnuts",
  "D", "14", "Deciduous fruits and nuts", "pistachios",
  "D", "15", "Deciduous fruits and nuts", "pomegranates",
  "D", "16", "Deciduous fruits and nuts", "plums prunes or apricots",
  "F", "1", "Field crops", "cotton",
  "F", "2", "Field crops", "safflower",
  "F", "10", "Field crops", "beans",
  "F", "11", "Field crops", "miscellaneous field",
  "F", "12", "Field crops", "sunflowers",
  "F", "16", "Field crops", "corn sorghum sudan",
  "G", "2", "Grain and hay crops", "wheat",
  "G", "6", "Grain and hay crops", "miscellaneous grain and hay",
  "I", "2", "Idle", "new prepped for production",
  "P", "1", "Pasture", "alfalfa and alfalfa mixtures",
  "P", "3", "Pasture", "mixed pasture",
  "P", "4", "Pasture", "native pasture",
  "P", "6", "Pasture", "miscellaneous grasses",
  "R", "1", "Rice", "rice",
  "R", "2", "Rice", "wild rice",
  "T", "4", "Truck, nursery, and berry crops", "cole crops",
  "T", "6", "Truck, nursery, and berry crops", "carrots",
  "T", "9", "Truck, nursery, and berry crops", "melons squash and cucumber",
  "T", "10", "Truck, nursery, and berry crops", "onions and garlic",
  "T", "12", "Truck, nursery, and berry crops", "potatoes",
  "T", "15", "Truck, nursery, and berry crops", "tomatoes",
  "T", "16", "Truck, nursery, and berry crops", "flowers nursery and christmas tree farms",
  "T", "18", "Truck, nursery, and berry crops", "miscellaneous truck",
  "T", "19", "Truck, nursery, and berry crops", "bush berries",
  "T", "20", "Truck, nursery, and berry crops", "strawberries",
  "T", "21", "Truck, nursery, and berry crops", "peppers",
  "T", "27", "Truck, nursery, and berry crops", "greenhouse",
  "T", "30", "Truck, nursery, and berry crops", "lettuce or leafy greens",
  "T", "31", "Truck, nursery, and berry crops", "potato or sweet potato",
  "V", "2", "Vineyards", "wine grapes",
  "YP", NA, "Young perennial", NA
)

# Create lookup key
mapping <- mapping %>%
  mutate(key = paste0(CLASS, SUBCLASS))

# Define lookup function
get_crop_name <- function(code) {
  out <- mapping$SUBCLASS_desc[mapping$key == code]
  if (length(out) == 0) NA else out
}
