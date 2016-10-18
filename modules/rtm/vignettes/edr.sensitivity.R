library(PEcAnRTM)

# Set paths and get files
# pecan.workflow.id <- "1000001494"
# ed.run.id <- "1000443342"
#pecan.workflow.id <- "1000001502"  # Worked, but low sensitivity
#ed.run.id <- "1000443579"
pecan.workflow.id <- "1000001543"
ed.run.id <- "1000443674"
analysis.path <- sprintf('analysis.output.%s.%s', pecan.workflow.id, ed.run.id)
dir.create(analysis.path)
exec.path <- file.path(analysis.path, "ed_2.1-opt")
if(!file.exists(exec.path)){
  file.link("/projectnb/dietzelab/ashiklom/ED2/EDR/build/ed_2.1-opt", exec.path)
  system(paste("chmod +x", exec.path))
}
base.output.dir <- file.path("/projectnb", "dietzelab", "pecan.data",
                             "output", "ashiklom")
ed2in.path <- file.path(base.output.dir, pecan.workflow.id,
                        "run", ed.run.id, "ED2IN")
history.path <- file.path(base.output.dir, pecan.workflow.id,
                          "out", ed.run.id)
paths <- list(ed2in.path = ed2in.path, history.path = history.path)
history.file <- tail(list.files(history.path, "history-S-.*"), 1)
date.raw <- gsub("history-S-(.*)-g01.h5", "\\1", history.file)
datetime <- strptime(date.raw, "%Y-%m-%d-%H%M%S", tz = "UTC")
if(lubridate::hour(datetime) < 8 | lubridate::hour(datetime) > 6) {
  lubridate::hour(datetime) <- 12
}

# Set sensitivity parameters
arg <- commandArgs(trailingOnly=TRUE)
if(length(arg) > 0){
    n.sens <- as.numeric(arg[1])
} else {
    n.sens <- 4
}
sensitivity.means <- list(
  prospect.N = 1.4,
  prospect.Cab = 40,
  prospect.Car = 7,
  prospect.Cw = 0.01,
  prospect.Cm = 0.007,
  ed.sla = 22,
  ed.b1bl = 0.08,
  ed.b2bl = 1,
  ed.b1bs = 0.147,
  ed.b2bs = 2.432,
  ed.b1ht = 0.4778,
  ed.b2ht = -0.3884,
  ed.clumping_factor = 0.85,
  ed.leaf_width = 0.068,
  ed.hgt_min = 0.5,
  ed.dbh_crit = 53.1459,
  ed.rho = 0.2,
  ed.leaf_scatter_vis = 0.21,
  ed.diffuse_backscatter_vis = 0.5,
  ed.emis_v = 0.95,
  ed.wood_reflect_vis = 0.12,
  ed.wood_reflect_nir = 0.25,
  ed.wood_trans_vis = 0.001,
  ed.wood_trans_nir = 0.001,
  ed.orient_factor = 0)

sensitivity.seqs <- list(
  prospect.N = seq(1, 4, length.out = n.sens),
  prospect.Cab = seq(10, 100, length.out = n.sens),
  prospect.Car = seq(2, 60, length.out = n.sens),
  prospect.Cw = c(seq(0.004, 0.01, length.out = n.sens/2),
                   seq(0.015, 0.05, length.out = n.sens/2)),
  prospect.Cm = c(seq(0.001, 0.005, length.out = n.sens/2),
                   seq(0.006, 0.015, length.out = n.sens/2)),
  ed.sla = c(seq(6, 50, length.out = n.sens)),
  ed.b1bl = c(seq(0.01, 0.08, length.out = n.sens/2),
              seq(0.1, 0.8, length.out = n.sens/2)),
  ed.b2bl = c(seq(0.9, 2, length.out = n.sens)),
  ed.b1bs = c(seq(0.00001, 0.3, length.out = n.sens)),
  ed.b2bs = c(seq(1, 3, length.out = n.sens)),
  ed.b1ht = c(seq(0.03, 0.5, length.out = n.sens/2),
              seq(1, 27, length.out = n.sens/2)),
  ed.b2ht = c(seq(-0.75, 0.75, length.out = n.sens)),
  ed.clumping_factor = seq(0.5, 1, length.out = n.sens),
  ed.leaf_width = seq(0.02, 0.15, length.out = n.sens),
  ed.hgt_min = seq(0.15, 2, length.out = n.sens),
  ed.dbh_crit = seq(0.6, 100, length.out = n.sens),
  ed.rho = seq(0, 0.9, length.out = n.sens),
  ed.leaf_scatter_vis = seq(0.1, 0.4, length.out = n.sens),
  ed.diffuse_backscatter_vis = seq(0.4, 0.7, length.out = n.sens),
  ed.emis_v = seq(0.9, 0.99, length.out = n.sens),
  ed.wood_reflect_vis = seq(0.05, 0.2, length.out = n.sens),
  ed.wood_reflect_nir = seq(0.15, 0.35, length.out = n.sens),
  ed.wood_trans_vis = seq(0.001, 0.4, length.out = n.sens),
  ed.wood_trans_nir = seq(0.001, 0.35, length.out = n.sens),
  ed.orient_factor = seq(0, 0.5, length.out = n.sens)
)

par.wl <- 400:2499
nir.wl <- 2500
prospect.version <- 5
pft.names <- c('Optics.Temperate_Early_Hardwood', 
               'Optics.Temperate_Mid_Hardwood',
               'Optics.Temperate_Late_Hardwood',
               'Optics.Temperate_Late_Conifer')

# Loop over traits
for(i in seq_along(sensitivity.means)){
  current.trait <- names(sensitivity.means)[i]
  albedo.mat <- matrix(NA, 2101, n.sens)
  for(j in 1:n.sens){
    # Set inputs
    print(sprintf("%s: %d of %d", current.trait, j, n.sens))
    prospect.in <- unlist(sensitivity.means[1:5])
    if(i <= 5){
      prospect.in[current.trait] <- sensitivity.seqs[[current.trait]][j]
      trait.in <- NULL
    } else {
      trait.in <- sensitivity.seqs[[current.trait]][j]
      names(trait.in) <- substring(current.trait, 4)  ## Remove "ed." from trait name
    }
    trait.values <- lapply(pft.names, function(x) c(list(name=x), trait.in))
    names(trait.values) <- pft.names
    albedo <- EDR.prospect(prospect.param = prospect.in,
                             prospect.version = prospect.version,
                             paths=paths, 
                             par.wl = par.wl, nir.wl = nir.wl,
                             datetime = datetime, 
                             trait.values = trait.values,
                             history.prefix = 'history',
                             change.history.time = TRUE,
                             output.path = analysis.path)
    albedo.diff <- c(0,diff(albedo))
    threshold <- 0.01
    albedo.na <- which(abs(albedo.diff) > threshold)
    albedo[albedo.na] <- NA
    albedo.mat[,j] <- albedo
  }
# Save output
  fname <- sprintf("%s/albedo.%s.rds", analysis.path, current.trait)
  saveRDS(albedo.mat, file=fname)
# Plot output
  png(sprintf("%s/%s.png", analysis.path, current.trait))
  plot.title <- sprintf("%s", current.trait)
  wl <- 400:2500
  matplot(wl, albedo.mat, type='l', main=plot.title)
  dev.off()
}

