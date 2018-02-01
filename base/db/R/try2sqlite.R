#' Convert TRY text file to SQLite database
#'
#' The TRY file is huge and unnecessarily long, which makes it difficult to 
#' work with. The resulting SQLite database is much smaller on disk, and can be 
#' read much faster thanks to lazy evaluation.
#'
#' The resulting TRY SQLite database contains the following tables:
#'  - `values` -- The actual TRY data. Links to all other tables through ID columns.
#'  - `traits` -- 
#'  - `datasets`
#'  - `species`
#'
#' @param try_files Character vector of file names containing TRY data.  
#' Multiple files will be `rbind`-ed together.
#' @param sqlite_file Target SQLite database file name, as character.
#' @export
try2sqlite <- function(try_files, sqlite_file = "try.sqlite") {
  # Read files
  raw_data <- Map(data.table::fread, try_files) %>%
    data.table::rbindlist()

  # Create integer reference ID for compact storage
  raw_data[, ReferenceID := as.integer(factor(Reference))]

  # Create tables
  data_cols <- c(
    "ObsDataID",        # TRY row ID -- unique to each observation of a given trait
    "ObservationID",    # TRY "entity" ID -- identifies a set of trait measurements (e.g. leaf)
    "DataID",           # Links to data ID
    "StdValue",         # Standardized, QA-QC'ed value
    "UnitName",         # Standardized unit
    "AccSpeciesID",     # Link to 'species' table
    "DatasetID",        # Link to 'datasets' table.
    "ReferenceID",      # Link to 'try_references' table.
    "ValueKindName",    # Type of value, e.g. mean, min, max, etc.
    "UncertaintyName",  # Kind of uncertainty
    "Replicates",       # Number of replicates
    "RelUncertaintyPercent",
    "OrigValueStr",         # Original data, as character string (before QA/QC)
    "OrigUnitStr",          # Original unit, as character string (before QA/QC)
    "OrigUncertaintyStr"    # Original uncertainty, as character string (before QA/QC)
  )
  data_values <- unique(raw_data[, data_cols, with = FALSE])
  data_values[, ]

  datasets_cols <- c(
    "DatasetID",
    "Dataset",
    "LastName",
    "FirstName",
    "Reference"
  )
  datasets_values <- unique(raw_data[, datasets_cols, with = FALSE])
  datasets_values[, doi := character()]   # Add DOI column, to be filled later

  traits_cols <- c(
    "DataID",
    "DataName",
    "TraitID",
    "TraitName"
  )
  traits_values <- unique(raw_data[, traits_cols, with = FALSE])

  species_cols <- c(
    "AccSpeciesID",
    "AccSpeciesName",
    "SpeciesName"
  )
  species_values <- unique(raw_data[, species_cols, with = FALSE])
  species_values[, BetySpeciesID := integer()]   # Add BETY ID column, to populate later

  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "values", data_values)
  DBI::dbWriteTable(con, "traits", data_values)
  DBI::dbWriteTable(con, "datasets", datasets_values)
  DBI::dbWriteTable(con, "species", species_values)

  NULL
}
