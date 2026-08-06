# Trait lookup + pool_calculations for planting / harvest events.

load_events_trait_pool <- function(pool_script) {
  pool_env <- new.env(parent = globalenv())
  source(pool_script, local = pool_env)
  harvest_csv <- Sys.getenv("HARVEST_LOOKUP_CSV", Sys.getenv("HARVEST_LOOKUP_RDS", ""))
  if (nzchar(harvest_csv)) {
    lk <- pool_env$load_trait_lookup(harvest_path = harvest_csv)
    message("[pool] Loaded trait lookup (harvest_path=", harvest_csv, ")")
  } else {
    lk <- pool_env$load_trait_lookup()
    message("[pool] Loaded trait lookup (default planting + harvest CSV paths)")
  }
  list(pool_env = pool_env, lk = lk)
}

harvest_destructive_default <- function() {
  tolower(Sys.getenv("HARVEST_WOODY_DESTRUCTIVE", "0")) %in% c("1", "true", "yes")
}

is_young_woody_harvest <- function(pft, class, specond = NA_character_) {
  if (!identical(tolower(trimws(as.character(pft))), "woody")) {
    return(FALSE)
  }
  cls <- toupper(trimws(as.character(class)))
  sp <- toupper(trimws(as.character(specond)))
  identical(cls, "YP") || identical(sp, "Y")
}

is_mature_woody <- function(pft, class, specond = NA_character_) {
  identical(tolower(trimws(as.character(pft))), "woody") &&
    !is_young_woody_harvest(pft, class, specond)
}

# CLASS-level woody stand removal / replant (subclass-only changes ignored).
woody_class_destroyed <- function(prior_pft, prior_class, prior_specond,
                                  curr_pft, curr_class, curr_specond) {
  if (!is_mature_woody(prior_pft, prior_class, prior_specond)) {
    return(FALSE)
  }
  if (is.na(curr_class) || !nzchar(trimws(as.character(curr_class)))) {
    return(TRUE)
  }
  if (!is_mature_woody(curr_pft, curr_class, curr_specond)) {
    return(TRUE)
  }
  !identical(
    toupper(trimws(as.character(prior_class))),
    toupper(trimws(as.character(curr_class)))
  )
}
