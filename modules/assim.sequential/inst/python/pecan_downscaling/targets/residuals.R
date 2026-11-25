# Yang residual fitting and three-hour member routing.
suppressPackageStartupMessages(library(data.table))

features <- c("t2m","sp","d2m","tp","strd","KGC","lon","sin_doy","cos_doy",
  "NDVI","EVI","twi","PH","Sand","lat","agb","SOC","N","year_since_disturb","PPFD",
  "WindSpeed","t2m2","PPFD2","EVI2","VPD_t2m","sincos","sm_0_10","sm_10_40",
  "sm_40_100","sm_100_200","VPD","is_day")
members <- sprintf("ens%02d", 1:25)
groups <- c(as.character(1:8), "EC")

yang_matrix <- function(data, levels, ec) {
  columns <- c(features, if (ec) "LC")
  frame <- as.data.frame(data[, ..columns])
  for (field in intersect(names(levels), columns)) {
    values <- as.character(frame[[field]])
    if (any(!is.na(values) & !values %in% levels[[field]])) stop("Unknown ", field)
    # A one-class domain has just one possible indicator. model.matrix
    # requires at least two factor levels to compute contrasts.
    frame[[field]] <- if (length(levels[[field]]) < 2L) {
      ifelse(is.na(values), NA_real_, 1)
    } else factor(values, levels = levels[[field]])
  }
  for (field in setdiff(columns, names(levels))) {
    if (is.logical(frame[[field]])) {
      if (any(!is.na(frame[[field]]))) stop("Unexpected logical predictor: ", field)
      frame[[field]] <- as.numeric(frame[[field]])
    }
    if (!is.numeric(frame[[field]])) stop("Non-numeric predictor: ", field)
  }
  formula <- reformulate(columns, intercept = FALSE)
  model.matrix(formula, model.frame(formula, frame, na.action = na.pass))
}

fit_group <- function(data, target, member, group, levels, threads) {
  raw <- data[[tolower(target)]]
  response <- raw - data[[member]]
  data <- data[is.finite(response)]
  setorder(data, index, utc)
  response <- data[[tolower(target)]] - data[[member]]
  ec <- group == "EC"
  if (nrow(data) < 10L) {
    if (ec) stop("Insufficient rows for EC model")
    return(list(model = NULL, levels = levels))
  }
  matrix <- yang_matrix(data, levels, ec)
  if (ec) {
    set.seed(2025L)
    training <- as.integer(caret::createDataPartition(response, p=.8, list=FALSE))
    validation <- setdiff(seq_along(response), training)
    train <- xgboost::xgb.DMatrix(matrix[training,,drop=FALSE], label=response[training])
    valid <- xgboost::xgb.DMatrix(matrix[validation,,drop=FALSE], label=response[validation])
    params <- list(objective="reg:squarederror", eta=.03, max_depth=12L, subsample=.85,
                   colsample_bytree=.95, min_child_weight=1, gamma=0, lambda=3, alpha=2, nthread=threads)
    model <- xgboost::xgb.train(params, train, nrounds=200, watchlist=list(train=train,eval=valid),
                                early_stopping_rounds=20, eval_metric="rmse", verbose=0)
  } else {
    set.seed(30000L + match(member, members))
    params <- list(objective="reg:squarederror", eta=.1, max_depth=6L, subsample=.8,
                   colsample_bytree=.8, min_child_weight=10, gamma=.5, lambda=3, alpha=2, nthread=threads)
    model <- xgboost::xgb.train(params, xgboost::xgb.DMatrix(matrix, label=response),
                                nrounds=200, verbose=0)
  }
  list(model=model, levels=levels, features=colnames(matrix))
}

read_means <- function(root, year) {
  path <- file.path(root, sprintf("year=%d.rds", year))
  a <- readRDS(path)
  if (!all(c("site_ids", "time", "nee", "le") %in% names(a)) ||
      !inherits(a$time, "POSIXct") || anyNA(a$time) || anyNA(a$site_ids) ||
      anyDuplicated(a$site_ids) || anyDuplicated(a$time) ||
      any(as.numeric(a$time) %% 10800 != 0) || any(format(a$time, "%Y", tz = "UTC") != year)) {
    stop("Invalid site or time axis: ", path)
  }
  for (key in c("nee", "le")) {
    if (!is.numeric(a[[key]]) || !identical(dim(a[[key]]), c(length(a$site_ids), length(a$time)))) {
      stop("Invalid ", key, " dimensions: ", path)
    }
  }
  a
}

read_grid <- function(path, means, matched, ids = NULL) {
  ds <- arrow::open_dataset(path)
  if (!is.null(ids)) ds <- dplyr::filter(ds, index %in% ids)
  grid <- as.data.table(dplyr::collect(ds))
  sites <- match(grid$index, means$site_ids)
  times <- match(as.numeric(grid$utc), as.numeric(means$time))
  if (anyNA(sites) || anyNA(times)) stop("Annual means missing site or time: ", path)
  at <- cbind(sites, times)
  grid[, nee := means$nee[at] * 1e8 / 1.157407]
  grid[, le := means$le[at]]
  for (key in c("nee", "le")) grid[!is.finite(get(key)) | abs(get(key)) > 1e6, (key) := NA_real_]
  grid[, Site_ID := matched$Site_ID[match(index, matched$index)]]
  grid
}

read_towers <- function(root, target, grid) {
  ids <- unique(na.omit(grid$Site_ID))
  if (!length(ids)) return(NULL)
  columns <- c("Site_ID", "utc", members)
  low <- min(grid$utc)
  high <- max(grid$utc)
  towers <- rbindlist(lapply(ids, function(site) {
    path <- file.path(root, paste0("target=", target), paste0("site=", site, ".parquet"))
    ds <- dplyr::filter(arrow::open_dataset(path), utc >= low, utc <= high)
    obs <- as.data.table(dplyr::collect(dplyr::select(ds, dplyr::all_of(columns))))
    if (!inherits(obs$utc, "POSIXct") || !identical(attr(obs$utc, "tzone"), "UTC") ||
        anyNA(obs$utc) || anyNA(obs$Site_ID) ||
        any(obs$Site_ID != site) || any(as.numeric(obs$utc) %% 10800 != 0) ||
        !all(vapply(obs[, ..members], is.numeric, logical(1)))) {
      stop("Invalid tower observations: ", path)
    }
    obs
  }))
  if (anyDuplicated(towers, by = c("Site_ID", "utc"))) stop("Duplicate tower observation: ", target)
  towers
}

fit_models <- function(paths, partitions, matched, scratch, model_dir) {
  if (!nrow(matched)) stop("Fitting Yang residuals requires matched tower sites")
  meta <- unique(rbindlist(lapply(partitions[year == min(year), path], function(path) {
    unique(as.data.table(arrow::read_parquet(path, col_select = c("index", "LC", "KGC"))))
  })))
  if (anyDuplicated(meta$index)) stop("Inconsistent site categories")
  levels <- lapply(setNames(groups, groups), function(g) {
    selected <- if (g == "EC") meta[index %in% matched$index] else if (as.integer(g) <= 4) meta[LC <= 4] else meta[LC >= 5]
    list(KGC = sort(unique(na.omit(selected$KGC))), is_day = c("0", "1"), LC = sort(unique(as.character(selected$LC))))
  })
  parts <- partitions[chunk %in% ((matched$index - 1L) %/% 500L + 1L)]
  for (y in unique(parts$year)) {
    means <- read_means(paths$site_means, y)
    for (i in which(parts$year == y)) {
      grid <- read_grid(parts$path[i], means, matched, matched$index)
      for (target in c("NEE", "LE")) {
        towers <- read_towers(paths$tower, target, grid)
        part <- towers[grid, on = .(Site_ID, utc)]
        raw <- tolower(target)
        columns <- unique(c("index", "utc", "LC", raw, features, members))
        part <- part[is.finite(get(raw)), ..columns]
        part <- part[rowSums(is.finite(as.matrix(part[, ..members]))) > 0]
        arrow::write_parquet(part, file.path(scratch, paste0(target, "_", parts$name[i], ".parquet")))
      }
    }
  }
  dir.create(model_dir)
  for (target in c("NEE", "LE")) {
    files <- file.path(scratch, paste0(target, "_", parts$name, ".parquet"))
    for (member in members) {
      columns <- unique(c("index", "utc", "LC", tolower(target), member, features))
      rows <- rbindlist(lapply(files, function(path) as.data.table(
        arrow::read_parquet(path, col_select = dplyr::all_of(columns)))))
      models <- lapply(setNames(groups, groups), function(g) {
        data <- if (g == "EC") rows else rows[LC == as.integer(g)]
        fit_group(data, target, member, g, levels[[g]], 1L)
      })
      saveRDS(models, file.path(model_dir, paste0(target, "_", member, ".rds")), compress = FALSE)
    }
  }
}

load_models <- function(dir, target) {
  lapply(members, function(member) {
    path <- file.path(dir, paste0(target, "_", member, ".rds"))
    bundles <- readRDS(path)
    if (!setequal(names(bundles), groups) || anyDuplicated(names(bundles))) stop("Invalid groups: ", path)
    lapply(bundles, function(b) {
      if (!all(c("model", "levels") %in% names(b)) ||
          !all(c("KGC", "is_day", "LC") %in% names(b$levels))) stop("Invalid residual bundle: ", path)
      if (!is.null(b$model)) {
        b$model <- xgboost::xgb.Booster.complete(b$model)
        if (!length(b$features) || !identical(b$features, b$model$feature_names)) {
          stop("Invalid model predictor order: ", path)
        }
        xgboost::xgb.parameters(b$model) <- list(nthread = 1L)
      }
      b
    })
  })
}

member_values <- function(batch, towers, models, target) {
  raw <- batch[[tolower(target)]]
  values <- matrix(rep(raw, length(members)), ncol = length(members))
  routes <- matrix(ifelse(is.finite(values), 3L, 0L), ncol = length(members))
  group <- ifelse(!is.na(batch$Site_ID), "EC", as.character(batch$LC))
  for (g in intersect(groups, group)) {
    ix <- which(group == g & is.finite(raw))
    if (!length(ix)) next
    levels <- NULL
    for (j in seq_along(members)) {
      b <- models[[j]][[g]]
      if (is.null(b$model)) next
      if (!identical(levels, b$levels)) {
        matrix <- yang_matrix(batch[ix], b$levels, g == "EC")
        levels <- b$levels
      }
      if (!identical(colnames(matrix), b$features)) stop("Predictor order mismatch: ", target, "/", g)
      corrected <- raw[ix] - predict(b$model, matrix)
      ok <- is.finite(corrected)
      values[ix[ok], j] <- corrected[ok]
      routes[ix[ok], j] <- 2L
    }
  }
  if (!is.null(towers)) {
    direct <- as.matrix(towers[batch[, .(Site_ID, utc)], on = .(Site_ID, utc), ..members])
    valid <- is.finite(direct)
    values[valid] <- direct[valid]
    routes[valid] <- 1L
  }
  out <- batch[, .(index, utc)]
  out[, raw := ..raw]
  out[, (members) := as.data.table(values)]
  out[, (sprintf("route%02d", 1:25)) := as.data.table(routes)]
  out
}

run_residuals <- function(spec, out, scratch, model_dir = NULL) {
  if (utils::packageVersion("xgboost") != "1.7.8.1") stop("Yang models require R xgboost 1.7.8.1")
  setDTthreads(1L)
  paths <- spec$paths
  partitions <- as.data.table(spec$partitions)
  matched <- fread(paths$matched_towers)
  matched <- matched[index %in% fread(paths$sites)$index & is.finite(min_dist_m) & min_dist_m <= 1000]
  if (anyNA(matched$Site_ID) || any(!nzchar(matched$Site_ID)) || any(matched$min_dist_m < 0)) {
    stop("Invalid matched tower record")
  }
  matched <- matched[order(min_dist_m), .SD[1], by = index]
  if (is.null(model_dir)) {
    model_dir <- file.path(out, "models")
    fit_models(paths, partitions, matched, scratch, model_dir)
  }
  for (target in c("NEE", "LE")) {
    models <- load_models(model_dir, target)
    for (y in unique(partitions$year)) {
      means <- read_means(paths$site_means, y)
      for (i in which(partitions$year == y)) {
        panel <- read_grid(partitions$path[i], means, matched)
        towers <- read_towers(paths$tower, target, panel)
        dest <- file.path(out, "members", partitions$name[i])
        dir.create(dest, recursive = TRUE, showWarnings = FALSE)
        arrow::write_parquet(member_values(panel, towers, models, target), file.path(dest, paste0(target, ".parquet")))
      }
    }
  }
}
if (sys.nframe() == 0L) {
  args <- commandArgs(TRUE)
  run_residuals(jsonlite::read_json(args[1], simplifyVector = TRUE), args[2], args[3],
                if (length(args) == 4L) args[4])
}
