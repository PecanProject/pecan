group_to_class <- c(
  # Citrus and subtropical
  "dates" = "Citrus and subtropical",
  "avocados" = "Citrus and subtropical",
  "olives" = "Citrus and subtropical",
  "miscellaneous subtropical fruits" = "Citrus and subtropical",
  "kiwis" = "Citrus and subtropical",
  
  # Deciduous fruits and nuts
  "apples" = "Deciduous fruits and nuts",
  "cherries" = "Deciduous fruits and nuts",
  "peaches and nectarines" = "Deciduous fruits and nuts",
  "pears" = "Deciduous fruits and nuts",
  "miscellaneous deciduous" = "Deciduous fruits and nuts",
  "mixed deciduous" = "Deciduous fruits and nuts",
  "almonds" = "Deciduous fruits and nuts",
  "walnuts" = "Deciduous fruits and nuts",
  "pistachios" = "Deciduous fruits and nuts",
  "pomegranates" = "Deciduous fruits and nuts",
  "plums prunes or apricots" = "Deciduous fruits and nuts",
  
  # Field crops
  "cotton" = "Field crops",
  "safflower" = "Field crops",
  "beans" = "Field crops",
  "miscellaneous field" = "Field crops",
  "sunflowers" = "Field crops",
  "corn sorghum sudan" = "Field crops",
  
  # Grain and hay crops
  "wheat" = "Grain and hay crops",
  "miscellaneous grain and hay" = "Grain and hay crops",
  
  # Pasture
  "alfalfa and alfalfa mixtures" = "Pasture",
  "mixed pasture" = "Pasture",
  "native pasture" = "Pasture",
  "miscellaneous grasses" = "Pasture",
  
  # Rice
  "rice" = "Rice",
  "wild rice" = "Rice",
  
  # Truck, nursery, and berry crops
  "cole crops" = "Truck, nursery, and berry crops",
  "carrots" = "Truck, nursery, and berry crops",
  "melons squash and cucumber" = "Truck, nursery, and berry crops",
  "onions and garlic" = "Truck, nursery, and berry crops",
  "potatoes" = "Truck, nursery, and berry crops",
  "tomatoes" = "Truck, nursery, and berry crops",
  "flowers nursery and christmas tree farms" = "Truck, nursery, and berry crops",
  "miscellaneous truck" = "Truck, nursery, and berry crops",
  "bush berries" = "Truck, nursery, and berry crops",
  "strawberries" = "Truck, nursery, and berry crops",
  "peppers" = "Truck, nursery, and berry crops",
  "greenhouse" = "Truck, nursery, and berry crops",
  "lettuce or leafy greens" = "Truck, nursery, and berry crops",
  "potato or sweet potato" = "Truck, nursery, and berry crops",
  
  # Vineyards
  "wine grapes" = "Vineyards",
  
  # Young perennial
  "NA" = "Young perennial"
)

map_group_to_class <- function(group_vec) {
  class <- group_to_class[group_vec]
  class[is.na(class)] <- "NA"
  return(unname(class))
}

# Convert named vector to dataframe
group_class_df <- data.frame(
  group = names(group_to_class),
  class = unname(group_to_class),
  row.names = NULL,
  stringsAsFactors = FALSE
)


