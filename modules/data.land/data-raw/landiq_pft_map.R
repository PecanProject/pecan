#!/usr/bin/env Rscript
# LandIQ PFT mapping
#
# This script creates a lookup table mapping LandIQ CLASS/SUBCLASS codes 
# to PFT categories for use in the package.

landiq_pft_map <- dplyr::tribble(
  ~CLASS, ~pft,
  # Woody perennial crops
  "C",  "woody",  # Citrus and subtropical
  "D",  "woody",  # Deciduous fruits and nuts
  "V",  "woody",  # Vineyards
  "YP", "woody",  # Young perennials
  

  # Herbaceous/row crops
  "F",  "row",    # Field crops
  "G",  "row",    # Grain and hay (predominantly row; some hay subclasses)
  "P",  "row",    # Pasture (treated as row/herbaceous)
  "T",  "row",    # Truck, nursery & berry crops (most are row)
  
  # Rice (special handling)
  "R",  "rice",   # Rice
  
  # Non-cropland (excluded from SIPNET runs)
  "I",  "idle",       # Idle land
  "S",  "semi-ag",    # Semi-agricultural
  "U",  "urban",      # Urban - generic
  "UC", "urban",      # Urban - commercial
  "UI", "urban",      # Urban - industrial
  "UL", "urban",      # Urban - lawn
  "UR", "urban",      # Urban - residential
  "UV", "urban",      # Urban - vacant
  "NB", "non-crop",   # Barren and wasteland
  "NC", "non-crop",   # Native class
  "NR", "non-crop",   # Riparian vegetation
  "NV", "non-crop",   # Native vegetation
  "NW", "non-crop",   # Water surface
  "E",  "non-crop",   # Entry denied
  "X",  "non-crop",   # Not cropped or unclassified
  "Z",  "non-crop"    # Outside study area
)

# Add SUBCLASS-level overrides where CLASS-level assignment is insufficient
# For T (Truck crops): Bush berries and blueberries are woody
landiq_pft_subclass_overrides <- dplyr::tribble(
  ~CLASS, ~SUBCLASS, ~pft,
  "T",    "19",      "woody",  # Bush berries
  "T",    "28",      "woody",  # Blueberries
  # G (Grain and hay): hay subclasses
  "G",    "6",       "hay",    # Miscellaneous grain and hay
  "G",    "7",       "hay"     # Mixed grain and hay
)

usethis::use_data(landiq_pft_map, overwrite = TRUE)
usethis::use_data(landiq_pft_subclass_overrides, overwrite = TRUE)