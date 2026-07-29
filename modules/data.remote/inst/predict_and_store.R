##loop through all the design points, predicts up until 2050 (or another specified end_year), and stores them in a 
#landIQ style dataset

setwd("/projectnb/dietzelab/ananyak")
library(data.table)
library(sf)
library(ggplot2)
library(tigris)

##-------setup-------
##read necessary files and check files are formatted properly
year_states = fread("year_states.csv")
tmat_df = fread("transition_matrix.csv")

row_id_col = colnames(tmat_df)[1]
states = colnames(tmat_df)[-1]

tmat_final = as.matrix(tmat_df[, -1, with = FALSE])
rownames(tmat_final) = tmat_df[[row_id_col]]
colnames(tmat_final) = states
storage.mode(tmat_final) = "double"

stopifnot(all(rownames(tmat_final) == colnames(tmat_final)))
setDT(year_states)
tmat_year = tmat_final 
start_info = year_states[, .SD[which.max(year)], by = parcel_id]

#clean classes and set in same order as transition matrix 
start_info[, dominant_crop := trimws(as.character(dominant_crop))]
states = rownames(tmat_final)

##specify how long into the future the user wants to predict to 
end_year = 2050

##--------design point predictions---------
design_points = fread('/projectnb/dietzelab/ccmmf/management/design_points_landiq_2018-2023.csv')

design_points[, parcel_id := as.character(parcel_id)]
start_info[, parcel_id := as.character(parcel_id)]
year_states[, parcel_id := as.character(parcel_id)]

#filter to only design point parcels
start_info = start_info[parcel_id %in% design_points$parcel_id]
year_states_design = year_states[parcel_id %in% design_points$parcel_id]

##function that pulls transition matrix probabilities
all_preds = start_info[, {
  
  current_state = dominant_crop 
  years = seq(year + 1, end_year)
  
  preds = character(length(years))
  probs = numeric(length(years))
  
  for (i in seq_along(years)) {
    p = tmat_year[current_state, ]
    
    if (sum(p) == 0) {
      preds[i] = NA_character_
      probs[i] = NA_real_
      next
    }
    
    idx = which.max(p)
    next_state = states[idx]
    
    preds[i] = next_state
    probs[i] = p[idx]
    
    current_state = next_state
  }
  
  .(
    year = years,
    pred_class = preds,
    pred_prob = probs
  )
  
}, by = parcel_id]

#store the actual classes from 2018-2023 
actual_hist = year_states_design[, .(parcel_id, year, pred_class = NA_character_, 
                                     actual_class = dominant_crop)]

#future predictions 
preds_future = all_preds[, .(
  parcel_id,
  year,
  pred_class,
  actual_class = NA_character_)]

##-----plotting------
#combine for comparisons 
plot_data = rbind(actual_hist, preds_future, fill = TRUE)

sample_pids = sample(unique(plot_data$parcel_id), 1)

plot_subset = plot_data[parcel_id %in% sample_pids]

plot_subset[, pred_class := factor(pred_class, levels = states)]
plot_subset[, actual_class := factor(actual_class, levels = states)]

ggplot(plot_subset, aes(x = year)) +
  
  geom_point(
    data = plot_subset[!is.na(pred_class)],
    aes(y = pred_class, color = pred_class),
    size = 3
  ) +
  
  geom_point(
    data = plot_subset[!is.na(actual_class)],
    aes(y = actual_class),
    shape = 1,
    size = 3,
    color = "black"
  ) +
  
  facet_wrap(~parcel_id, ncol = 2) +
  
  scale_x_continuous(
    limits = c(2018, end_year),
    breaks = seq(2018, end_year, by = 2)
  ) +
  
  ggtitle(sprintf("Predicted after Actual crop classes for parcel %s", sample_pids))+
  theme_minimal()

##-------store predictions in landIQ style-------
##refers back to sublcass data and assigns the predictions a given sublcass based on fequencies
set.seed(123)  

#cleaning the original design point and prediction columns to be safe 
design_points[, CLASS := as.character(CLASS)]
design_points[, SUBCLASS := as.character(SUBCLASS)]
preds_future[, parcel_id := as.character(parcel_id)]
preds_future[, pred_class := as.character(pred_class)]

#getting the last observed landiq row for each parcel for current class/subclass before predictions start
last_obs = design_points[
  order(year, season),
  .SD[.N],
  by = parcel_id
][
  , .(
    parcel_id,
    last_CLASS = CLASS,
    last_SUBCLASS = SUBCLASS
  )
]

#subclass probability table: if a class transition happens, draw subclass based on observed frequencies
subclass_probs = design_points[
  !is.na(CLASS) & !is.na(SUBCLASS),
  .N,
  by = .(CLASS, SUBCLASS)
]

subclass_probs[, prob := N / sum(N), by = CLASS]

draw_subclass = function(class_name) {
  
  choices = subclass_probs[CLASS == class_name]
  
  if (nrow(choices) == 0) {
    return(NA_character_)
  }
  
  sample(
    choices$SUBCLASS,
    size = 1,
    prob = choices$prob
  )
}

#adding previous observed class/subclass to predictions
preds_subclass = merge(
  preds_future[, .(parcel_id, year, CLASS = pred_class)],
  last_obs,
  by = "parcel_id",
  all.x = TRUE
)

setorder(preds_subclass, parcel_id, year)

preds_subclass[, SUBCLASS := NA_character_]

#assigning subclass year by year
for (p in unique(preds_subclass$parcel_id)) {
  
  idxs = which(preds_subclass$parcel_id == p)
  
  prev_class = preds_subclass$last_CLASS[idxs[1]]
  prev_subclass = preds_subclass$last_SUBCLASS[idxs[1]]
  
  for (i in idxs) {
    
    current_class = preds_subclass$CLASS[i]
    
    if (is.na(current_class)) {
      preds_subclass$SUBCLASS[i] = NA_character_
      
    } else if (!is.na(prev_class) && current_class == prev_class) {
      
      #if a crop class does not transition, keep the subclass its currently in 
      preds_subclass$SUBCLASS[i] = prev_subclass
      
    } else {
      
      #if crop class transitions, draw subclass from new class distribution
      preds_subclass$SUBCLASS[i] = draw_subclass(current_class)
    }
    
    #update previous class/subclass for next predicted year
    prev_class = current_class
    prev_subclass = preds_subclass$SUBCLASS[i]
  }
}

#adding class/subclass descriptions from lookup table
lookup = fread('/projectnb/dietzelab/ccmmf/management/LandIQ_cropCode_lookup_table.csv')

lookup[, CLASS := as.character(CLASS)]
lookup[, SUBCLASS := as.character(SUBCLASS)]

lookup_subclass = unique(lookup[, .(
  CLASS,
  SUBCLASS,
  CLASS_desc,
  SUBCLASS_desc,
  PFT
)])

preds_subclass = merge(
  preds_subclass,
  lookup_subclass,
  by = c("CLASS", "SUBCLASS"),
  all.x = TRUE
)

#adding parcel information from original design points
parcel_meta = design_points[
  order(year, season),
  .SD[.N],
  by = parcel_id
][
  , .(parcel_id, site_id, lon, lat)
]

preds_landiq = merge(
  preds_subclass,
  parcel_meta,
  by = "parcel_id",
  all.x = TRUE
)

#leaving season blank for future years 
preds_landiq[, season := NA_integer_]

#keep the columns that are in the design points file 
preds_landiq = preds_landiq[, .(
  site_id,
  parcel_id,
  lon,
  lat,
  year,
  season,
  CLASS,
  SUBCLASS,
  CLASS_desc,
  SUBCLASS_desc,
  PFT
)]

#saving observed and predicted crop classes for the design points as separate files 

design_points_observed = copy(design_points[year >= 2018 & year <= 2023])
design_points_predicted = copy(preds_landiq[year >= 2024 & year <= end_year])

design_points_observed[, source := "observed"]
design_points_predicted[, source := "predicted"]

setorder(design_points_observed, parcel_id, year, season)
setorder(design_points_predicted, parcel_id, year, season)

fwrite(
  design_points_observed,
  "design_points_observed.csv"
)

fwrite(
  design_points_predicted,
  sprintf("predicted_2024_%s.csv", end_year)
)





