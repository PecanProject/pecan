#start from original transition matrix (tmat_final) and build initial state vector for all classes to find the new optimized 
#transition matrix based on a user specified goal and given constraints. 

library(data.table)
library(nloptr)
library(expm) 
library(dplyr) 
library(ggplot2) 
library(reshape2)
library(networkD3) 

setwd("/projectnb/dietzelab/ananyak")

##------------setup-----------------
#load transition matrix and reformat 
  #keep row labels/states before dropping first column
  #drop V1/class column to keep numeric matrix
  #check matrix is square and rows sum to 1
tmat_df = fread("transition_matrix.csv")

row_id_col = colnames(tmat_df)[1]
states = colnames(tmat_df)[-1]

A_orig = as.matrix(tmat_df[, ..states])
rownames(A_orig) = tmat_df[[row_id_col]]
colnames(A_orig) = states
storage.mode(A_orig) = "double"

n = nrow(A_orig)

stopifnot(nrow(A_orig) == ncol(A_orig))
stopifnot(all(rownames(A_orig) == colnames(A_orig)))
stopifnot(all(abs(rowSums(A_orig) - 1) < 1e-8))

#have to reread original data and add back acres column for initial state vector
path_management = "/projectnb/dietzelab/ccmmf/management"
path_landiq_v4  = "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1"

lookup = fread(file.path(path_management, "LandIQ_cropCode_lookup_table.csv"))
ag_classes = unique(lookup[is_agricultural == TRUE, as.character(CLASS)])

year_min = 2018L
year_max = 2023L

crops_full = as.data.table(
  arrow::open_dataset(file.path(path_landiq_v4, "crops_all_years.parq")) |>
    filter(year >= year_min, year <= year_max, CLASS %in% ag_classes) |>
    select(parcel_id, year, season, CLASS, SUBCLASS, ACRES) |>
    collect()
)

crops_full[, `:=`(
  parcel_id = as.character(parcel_id),
  year = as.integer(year),
  season = as.integer(season),
  CLASS = as.character(CLASS),
  SUBCLASS = as.character(SUBCLASS),
  ACRES = as.integer(ACRES)
)]

setDT(crops_full)

##filter to 2023 only (last observed year = prediction starting year)
crops_full_2023 = crops_full[year == 2023]

#sum total acres by class, then convert to %
total_land = sum(crops_full_2023$ACRES, na.rm = TRUE)
land_by_class = aggregate(ACRES ~ CLASS, data = crops_full_2023, FUN = sum, na.rm = TRUE)
land_by_class$class_land_percs = land_by_class$ACRES / total_land

#X0 has to be in the same order as the transition matrix states
X0_named = setNames(land_by_class$class_land_percs, land_by_class$CLASS)

#reorder to exactly match transition matrix states
X0 = X0_named[states]

#classes in tmat but missing in 2023 get 0
X0[is.na(X0)] = 0

stopifnot(all(names(X0) == states))
stopifnot(abs(sum(X0) - 1) < 1e-8)

##drop v1 column from tmat for exact ordering
tmat_df = tmat_df %>% select(-V1)

##-----------scenario goal inputs-----------

#example: x% increase in crop class __ after n years/steps
target_crop = "V"

target_val = X0[target_crop] * 1.30
steps = 15 

##---------objective and constraints-------------
  #1.Minimize the "distance" from the original matrix
  #2.x is a vector of the matrix elements, length n^2

obj_fun = function(x) {
  A_new = matrix(x, nrow = n, byrow = TRUE)
  sum((A_new - A_orig)^2)
}

constr_fun = function(x) {
  A_new = matrix(x, nrow = n, byrow = TRUE)
  
  X_end = X0 %*% (A_new %^% steps)
  colnames(X_end) = states
  
  target_const = X_end[1, target_crop] - target_val
  row_sums_const = rowSums(A_new) - 1
  
  c(target_const, row_sums_const)
}

##-----------run optimizer------------------
#starting point = original matrix flattened into a vector
init_x = as.vector(t(A_orig))

#COBYLA for non-linear constraints without needing derivatives
res = nloptr(x0 = init_x,
              eval_f = obj_fun,
              eval_g_eq = constr_fun,
              lb = rep(0, n^2), # Probabilities can't be negative
              ub = rep(1, n^2), # Probabilities can't exceed 1
             opts = list(
               algorithm = "NLOPT_LN_COBYLA",
               xtol_rel = 1e-5,
               ##how many max iterations 
               maxeval = 25000,
               print_level = 1
             ))

##-------outputs--------

A_final = matrix(res$solution, nrow = n, byrow = TRUE)
rownames(A_final) = states
colnames(A_final) = states

X_end_orig = X0 %*% (A_orig %^% steps)
X_end_final = X0 %*% (A_final %^% steps)

colnames(X_end_orig) = states
colnames(X_end_final) = states

print("Optimizer status:")
print(res$status)
print(res$message)

print("Optimized Matrix A:")
print(round(A_final, 4))

target_crop
X0[target_crop]
target_val
X_end_orig[1, target_crop]
X_end_final[1, target_crop]

round(rowSums(A_final), 8)
max(abs(A_final - A_orig))

constr_fun(res$solution)

##--------------visualizations------------------

## time series distributions
get_dist_over_time = function(X0, A, steps, states, scenario_name) {
  out = lapply(0:steps, function(t) {
    if (t == 0) {
      Xt = X0
    } else {
      Xt = as.numeric(X0 %*% (A %^% t))
    }
    
    data.table(
      step = t,
      year = 2023 + t,
      CLASS = states,
      prop_land = as.numeric(Xt),
      scenario = scenario_name
    )
  })
  
  rbindlist(out)
}

dist_orig = get_dist_over_time(X0, A_orig, steps, states, "Original transition matrix")
dist_final = get_dist_over_time(X0, A_final, steps, states, "Optimized transition matrix")

dist_all = rbind(dist_orig, dist_final)

#1.stacked area chart of projected land distribution by crop class (from optimized matrix)
##*changes are pretty small, so this graph is better for showing the final overall distribution, not comparisons 
ggplot(dist_final,
       aes(x = year, y = prop_land, fill = CLASS)) +
  geom_area() +
  labs(
    title = "Optimized projected land distribution by crop class",
    x = "Year",
    y = "Fraction of total land",
    fill = "Class"
  ) +
  theme_minimal()

#2.differences in original vs optimized land share 
final_compare = merge(
  dist_orig[step == steps, .(CLASS, orig_prop = prop_land)],
  dist_final[step == steps, .(CLASS, opt_prop = prop_land)],
  by = "CLASS"
)

final_compare[, change := opt_prop - orig_prop]

ggplot(final_compare,
       aes(x = reorder(CLASS, change), y = change)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Change in projected land share after", steps, "steps"),
    x = "Class",
    y = "Optimized - Original"
  ) +
  theme_minimal()

#3.change in target class (isolated graph) 
focus = dist_all[CLASS == target_crop]

ggplot(focus, aes(x = year, y = prop_land, color = scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = sprintf("Projected land share for class %s", target_crop),
    x = "Year",
    y = "Fraction of total land",
    color = "Scenario"
  ) +
  theme_minimal()

#-------------------sankey diagram-------------------------#
#where the 2023 land distribution ends up after the target number of steps 
#under the optimized transition matrix

A_use = A_final
scenario_name = "Optimized"

#n-step transition matrix
A_steps = A_use %^% steps

#flow table from 2023 class to final class
flows = as.data.table(as.table(A_steps))
setnames(flows, c("source_class", "target_class", "transition_prob"))
flows[, source_land := as.numeric(X0[source_class])]
flows[, value := source_land * transition_prob]

#**remove tiny flows so sankey is readable
flows = flows[value > 0.001]

#*make separate node labels for start and end
flows[, source := paste0(source_class, " 2023")]
flows[, target := paste0(target_class, " ", 2023 + steps)]

nodes = data.table(name = unique(c(flows$source, flows$target)))

flows[, source_id := match(source, nodes$name) - 1]
flows[, target_id := match(target, nodes$name) - 1]

links = flows[, .(
  source = source_id,
  target = target_id,
  value = value
)]

sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 13,
  nodeWidth = 25,
  sinksRight = TRUE
)

#####isolated sankey with only the target crop after time steps  

A_steps = A_final %^% steps

flows = as.data.table(as.table(A_steps))
setnames(flows, c("source_class", "target_class", "transition_prob"))

flows[, source_land := as.numeric(X0[source_class])]
flows[, value := source_land * transition_prob]

# only show land ending in target class
flows = flows[target_class == target_crop]

flows[, source := paste0(source_class, " 2023")]
flows[, target := paste0(target_class, " ", 2023 + steps)]

nodes = data.table(name = unique(c(flows$source, flows$target)))

flows[, source_id := match(source, nodes$name) - 1]
flows[, target_id := match(target, nodes$name) - 1]

links = flows[, .(
  source = source_id,
  target = target_id,
  value = value
)]

sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 13,
  nodeWidth = 25,
  sinksRight = TRUE
)

##-----predictions with the new optimized matrix-----

tmat_year = A_final
states = rownames(tmat_year)

year_states = fread("year_states.csv")
setDT(year_states)

year_states[, parcel_id := as.character(parcel_id)]
year_states[, dominant_crop := trimws(as.character(dominant_crop))]

start_info = year_states[, .SD[which.max(year)], by = parcel_id]

design_points = fread('/projectnb/dietzelab/ccmmf/management/design_points_landiq_2018-2023.csv')
design_points[, parcel_id := as.character(parcel_id)]

start_info = start_info[parcel_id %in% design_points$parcel_id]
year_states_design = year_states[parcel_id %in% design_points$parcel_id]

end_year = 2050

all_preds_optimized = start_info[, {
  
  current_state = dominant_crop
  years = seq(year + 1, end_year)
  
  preds = character(length(years))
  probs = numeric(length(years))
  
  for (i in seq_along(years)) {
    
    p = tmat_year[current_state, ]
    
    if (sum(p) == 0 || all(is.na(p))) {
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

preds_future = all_preds_optimized[, .(
  parcel_id,
  year,
  pred_class,
  actual_class = NA_character_
)]

##-----make similar graphs using optimized scenarios-----
#(same script format as initial prediction code)

actual_hist = year_states_design[, .(
  parcel_id,
  year,
  pred_class = NA_character_,
  actual_class = dominant_crop
)]

plot_data_optimized = rbind(actual_hist, preds_future, fill = TRUE)

sample_pids = sample(unique(plot_data_optimized$parcel_id), 1)

plot_subset_optimized = plot_data_optimized[parcel_id %in% sample_pids]

plot_subset_optimized[, pred_class := factor(pred_class, levels = states)]
plot_subset_optimized[, actual_class := factor(actual_class, levels = states)]

ggplot(plot_subset_optimized, aes(x = year)) +
  
  geom_point(
    data = plot_subset_optimized[!is.na(pred_class)],
    aes(y = pred_class, color = pred_class),
    size = 3
  ) +
  
  geom_point(
    data = plot_subset_optimized[!is.na(actual_class)],
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
  
  labs(
    title = sprintf(
      "Optimized scenario predictions after actual crop classes for parcel %s",
      sample_pids
    ),
    x = "Year",
    y = "Crop class",
    color = "Predicted class"
  ) +
  
  theme_minimal()

##-------store optimized predictions in landIQ style-------
#(still same script format as initial prediction code)

design_points[, CLASS := as.character(CLASS)]
design_points[, SUBCLASS := as.character(SUBCLASS)]
preds_future[, parcel_id := as.character(parcel_id)]
preds_future[, pred_class := as.character(pred_class)]

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

preds_subclass_optimized = merge(
  preds_future[, .(parcel_id, year, CLASS = pred_class)],
  last_obs,
  by = "parcel_id",
  all.x = TRUE
)

setorder(preds_subclass_optimized, parcel_id, year)

preds_subclass_optimized[, SUBCLASS := NA_character_]

for (p in unique(preds_subclass_optimized$parcel_id)) {
  
  idxs = which(preds_subclass_optimized$parcel_id == p)
  
  prev_class = preds_subclass_optimized$last_CLASS[idxs[1]]
  prev_subclass = preds_subclass_optimized$last_SUBCLASS[idxs[1]]
  
  for (i in idxs) {
    
    current_class = preds_subclass_optimized$CLASS[i]
    
    if (is.na(current_class)) {
      preds_subclass_optimized$SUBCLASS[i] = NA_character_
      
    } else if (!is.na(prev_class) && current_class == prev_class) {
      
      # if class does not change, keep previous subclass
      preds_subclass_optimized$SUBCLASS[i] = prev_subclass
      
    } else {
      
      # if class changes, draw subclass from observed subclass distribution
      preds_subclass_optimized$SUBCLASS[i] = draw_subclass(current_class)
    }
    
    prev_class = current_class
    prev_subclass = preds_subclass_optimized$SUBCLASS[i]
  }
}

#add class/subclass descriptions
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

preds_subclass_optimized = merge(
  preds_subclass_optimized,
  lookup_subclass,
  by = c("CLASS", "SUBCLASS"),
  all.x = TRUE
)

parcel_meta = design_points[
  order(year, season),
  .SD[.N],
  by = parcel_id
][
  , .(parcel_id, site_id, lon, lat)
]

preds_landiq_optimized = merge(
  preds_subclass_optimized,
  parcel_meta,
  by = "parcel_id",
  all.x = TRUE
)

preds_landiq_optimized[, season := NA_integer_]

# keep LandIQ-style columns
preds_landiq_optimized = preds_landiq_optimized[, .(
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

design_points_predicted_optimized = copy(
  preds_landiq_optimized[year >= 2024 & year <= end_year]
)

design_points_predicted_optimized[, source := "predicted_optimized"]

setorder(design_points_predicted_optimized, parcel_id, year, season)

fwrite(
  design_points_predicted_optimized,
  sprintf(
    "predicted_optimized_%s_2024_%s.csv",
    target_crop,
    end_year
  )
)