# 6. Add TRY data to BETY

# a. Loop over entities...
#   i. Add entity to entities table
#     name = TRY_OBSERVATION_<ObservationID>
#     notes = DatasetID, Dataset, ObservationID
#   ii. Store entity_id.
#   iii. Loop over rows...
#     1. INSERT INTO traits(...) VALUES (...)
#       site_id --> site_id
#       specie_id --> specie_id
#       citation_id --> citation_id
#       mean --> StdValue
#       n --> Replicates (if present)
#       user_id --> user_id
#       entity_id --> entity_id
#       variable_id --> bety_id
#       notes --> paste("TRY_VALUE", DatasetID, ObservationID, DataID, ObsDataID)
#       date_year --> from measurement date
#       date_month --> ^^
#       date_day --> ^^
#       time_hour --> from measurement time
#       time_minute --> ^^
#       created_at --> NOW()
#       updated_at --> NOW()
#     2. Store ID at every time step -- match with ObsDataID of TRY? Not perfect because miss time, etc., but may help later.