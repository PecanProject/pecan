##makes simple, 3x3  transition matrices for tillage states for each county

setwd("/projectnb/dietzelab/ananyak")
library(data.table)
library(arrow) 

dir_till = "/projectnb/dietzelab/ccmmf/management/event_files"

years = 2018:2023

##----load files----
#tillage from event files 
till_files = unlist(lapply(years, function(yr) {
  list.files(path = dir_till, pattern = paste0("^tillage_statewide_", yr, "\\.parquet$"), full.names = TRUE)}))

tillage = rbindlist(lapply(till_files, function(f) {
    dt = as.data.table(read_parquet(f))
    
    yr = as.integer(sub(".*tillage_statewide_([0-9]{4})\\.parquet$", "\\1", basename(f)))
    
    dt[, year := yr]
    dt}),
  fill = TRUE)

setorder(tillage, parcel_id, year)

##-----edit/filter tillage-----
#add till class column based on ndti_pct_change: 0-30 = no till, 30-69 = low till, 70+ = high 

tillage[, till_class := fifelse( ndti_pct_change >= 0 & ndti_pct_change < 30, "no_till",
                                 fifelse(ndti_pct_change >= 30 & ndti_pct_change < 70, "low_till",
                                         fifelse(ndti_pct_change >= 70, "high_till", NA_character_)))]

##-----p&h dates----
phenology_dir = "/projectnb/dietzelab/ccmmf/management/phenology/matched_landiq_mslsp_v4.1"
phenology_files = list.files(phenology_dir, pattern = "^assigned_year=.*\\.parquet$", full.names = TRUE)

#combining them all 
assigned_all = rbindlist(lapply(phenology_files, function(f) {
    dt = as.data.table(read_parquet(f))
    #pull year from filename if year column is not already there
    yr = as.integer(gsub(".*assigned_year=([0-9]{4})\\.parquet$", "\\1", f))
    dt[, source_year := yr]
    
    return(dt)}),
  fill = TRUE)

##----load cleaned crop year states from original transition matrix script----
#file already has dominant crop class per parcel&year w/ the same workflow as transition_matrix.R

crop_year = fread("/projectnb/dietzelab/ananyak/crop_year_states_cleaned.csv")

crop_year[, `:=`(
  parcel_id = as.character(parcel_id), year = as.integer(year), crop_class = as.character(state), 
  crop_non_dom_prob = non_dom_prob, ACRES = as.integer(ACRES))]

crop_year = crop_year[
  ,
  .(
    parcel_id, year, county, county_geoid, crop_class, crop_non_dom_prob, ACRES)]

#transition matrix 
##----create annual tillage states-----

tillage[, parcel_id := as.character(parcel_id)]
tillage[, year := as.integer(year)]

tillage_counts = tillage[
  !is.na(till_class),
  .N, by = .(parcel_id, year, till_class)]

tillage_counts[, total_obs := sum(N), by = .(parcel_id, year)]

setorder(tillage_counts, parcel_id, year, -N)

tillage_year_states = tillage_counts[,
                                     .SD[1], by = .(parcel_id, year)][,
                                                                      .(
                                                                        parcel_id, year, state = till_class, n_obs = N, total_obs, non_dom_prob = 1 - N / total_obs)]

##----merge crop class onto tillage states----
## to get tillage and crop class per parcel & year

tillage_year_states = merge(tillage_year_states, crop_year, by = c("parcel_id", "year"), all.x = TRUE)

setorder(tillage_year_states, crop_class, parcel_id, year)

write.csv(tillage_year_states, 'all_data.csv')

##----use transition format & matrix functions----
source("/projectnb/dietzelab/ananyak/transition_functions.R")

states = c("no_till", "low_till", "high_till")

tillage_transitions_annual = make_transitions(year_states = tillage_year_states, id_col = "parcel_id", time_col = "year",
                                              state_col = "state", non_dom_col = "non_dom_prob")

#overall annual tillage transition matrix
till_mat = make_transition_matrix(
  dt = tillage_transitions_annual,
  states_all = states)

till_mat

#annual tillage transition matrices by county
transition_matrix_classes = make_grouped_transition_matrices(
  transitions = tillage_transitions_annual[!is.na(crop_class)],
  states_all = states,
  group_cols = c("county"))

dir.create("county_till_matrices", showWarnings = FALSE)

for (cty in names(transition_matrix_classes)) {
  safe_cty = gsub("[^A-Za-z0-9_-]", "_", cty)
  write.csv(transition_matrix_classes[[cty]],
            file = file.path("county_till_matrices", paste0(safe_cty, "_till_matrix.csv")),
            row.names = TRUE)}
