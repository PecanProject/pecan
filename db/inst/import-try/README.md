---
title: "Import TRY database into BETY""
Author: "Alexey Shiklomanov"
---

# Package dependencies
1. `data.table` -- Makes it remotely possible to work with the TRY database. Requires an up-to-date version, so if parts of the workflow break, try re-installing this.
2. `bit64` -- Used by `data.table` to read and store large integers, which constitude most of the ID's in TRY and BETY.
3. `rcrossref` -- Citation matching service. Used to programmatically grab TRY reference DOI's.
4. `PEcAn.DB` -- For interfacing with BETY database.
5. `RPostgreSQL` -- Also for interfacing with BETY.

With a recent-enough R version (> 3.2), we can bring in the following:
1. `taxize` -- Powerful taxanomic name resolution service.


# Workflow
  1. Global subset
    a. Select only standardized values -- !is.na(StdValue)
    b. Merge with TRY-BETY translation and select only data that have a match -- !is.na(bety_name) OR Measurement Date (DataID = 241) and Time (DataID = 394).
    c. Select only observation IDs that have at least one trait value -- any(type == "t"), by=ObservationID

  2. Data-specific subset
    a. Deal with datasets that report values that are anything other than "Single"
  
  3. Create TRY sites in BETY.
    a. Cast latitudes, longitudes, and data names
    b. Assign each unique lat-lon pair a unique location.id -- := .GRP, by=list(Latitude,Longitude)
    c. Determie sites using cluster analysis, and create unique site.id
    d. Append location.id and site.id to full data set by merging on ObservationID
    e. Create data.table for site creation:
        sitename = "TRY_SITE_<site.id>"
        notes = "TRY_DATASETS = <dataset IDs>"
        geometry = ST_GeomFromText('POINT(lat, lon)', 4263)
    f. Loop over rows. Check site centroid against BETY. Create new sites if necessary. Append "site_id" to full data.table at every step.
        NOTE: In the future, change centroid to bounding box containing all sites?
  
  4. Match species between TRY and BETY
    a. Get unique species list from TRY
    b. Loop over TRY species list, and add BETY species ID to TRY data.table. For each:
      i. Try a naive merge (SELECT id,scientificname FROM species WHERE scientificname = AccSpeciesName)
      ii. Try a fuzzy merge (SELECT id,scientificname FROM species WHERE scientificname LIKE AccSpeciesName)
      iii. Insert the species?
    c. Merge species ID into main TRY data.table
  
  5. Add TRY citations into BETY
    a. Get unique citation list from TRY.
    b. Get data for each citation via rcrossref (author, year, title, journal, volume, pg, url, doi)
    c. Loop over DOI's:
        i. Check if DOI already in BETY. If yes, record citation_id
        ii. If no, INSERT INTO citations(author, year, title, journal, vol, pg, url, doi) VALUES(...)
        iii. Get last citation ID.
    d. Merge citation_id back into base TRY data.

6. Add TRY data to BETY
    a. Loop over entities...
      i. Add entity to entities table
        name = TRY_OBSERVATION_<ObservationID>
        notes = DatasetID, Dataset, ObservationID
      ii. Store entity_id.
      iii. Loop over rows...
        1. INSERT INTO traits(...) VALUES (...)
          site_id --> site_id
          specie_id --> specie_id
          citation_id --> citation_id
          mean --> StdValue
          n --> Replicates (if present)
          user_id --> user_id
          entity_id --> entity_id
          variable_id --> bety_id
          notes --> paste("TRY_VALUE", DatasetID, ObservationID, DataID, ObsDataID)
          date_year --> from measurement date
          date_month --> ^^
          date_day --> ^^
          time_hour --> from measurement time
          time_minute --> ^^
          created_at --> NOW()
          updated_at --> NOW()
        2. Store ID at every time step -- match with ObsDataID of TRY? Not perfect because miss time, etc., but may help later.
      
