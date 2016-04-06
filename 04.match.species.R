# 4. Match species between TRY and BETY

# a. Get unique species list from TRY
# b. Loop over TRY species list, and add BETY species ID to TRY data.table. For each:
#   i. Try a naive merge (SELECT id,scientificname FROM species WHERE scientificname = AccSpeciesName)
#   ii. Try a fuzzy merge (SELECT id,scientificname FROM species WHERE scientificname LIKE AccSpeciesName)
#   iii. Insert the species?
# c. Merge species ID into main TRY data.table