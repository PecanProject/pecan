#!/usr/bin/env Rscript

library(ggplot2)

config <- config::get(file = "workflows/sipnet-restart-workflow/config.yml")

outdir_root <- config[["outdir_root"]]

settings <- PEcAn.settings::read.settings(file.path(outdir_root, "settings.xml"))
events_json_file <- settings |>
  purrr::chuck("run", "inputs", "events", "source", "path1")

events <- jsonlite::read_json(events_json_file, simplifyVector = FALSE)

events_df <- dplyr::bind_rows(events[[1]][["events"]]) |>
  dplyr::mutate(date = as.Date(.data$date)) |>
  dplyr::filter(.data$event_type != "irrigation") |>
  dplyr::arrange(.data$date)

modeloutdir <- file.path(config$outdir_root, "output", "out")
runids <- list.files(modeloutdir)

vars_some <- c("NEE", "LAI", "AGB", "TotSoilCarb", "leaf_carbon_content", "litter_carbon_content")
vars_more <- c(
  "GPP", "NPP", "TotalResp", "AutoResp", "HeteroResp", "SoilResp", "NEE", "AbvGrndWood",
  "leaf_carbon_content", "TotLivBiom", "TotSoilCarb", "Qle", "Transp", "SoilMoist", "SoilMoistFrac",
  "litter_carbon_content", "LAI", "fine_root_carbon_content", "coarse_root_carbon_content",
  "AGB", "GWBI"
  # "mineral_N", "soil_organic_N", "litter_N", "N2O_flux", "N_leaching", "N_fixation", "N_uptake", "CH4_flux", "SWE"
)

read_output <- function(modeloutdir, runid, variables = vars_some) {
  PEcAn.utils::read.output(
    runid,
    file.path(modeloutdir, runid),
    variables = variables,
    dataframe = TRUE
  ) |>
    dplyr::mutate(run_id = .env$runid) |>
    dplyr::as_tibble()
}

results <- purrr::map(runids, read_output, modeloutdir = modeloutdir, variables = vars_more) |>
  dplyr::bind_rows()
plt <- results |>
  tidyr::pivot_longer(
    -c("posix", "year", "run_id"),
    names_to = "variable",
    values_to = "value"
  ) |>
  ggplot() +
  aes(x = posix, y = value, color = run_id) +
  geom_line() +
  geom_vline(
    aes(xintercept = date, linetype = event_type),
    data = events_df
  ) +
  scale_linetype_manual(values = c("planting" = "dotted", "harvest" = "dashed")) +
  facet_wrap(vars(variable), scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom")
if (interactive()) plt

ggsave(
  file.path(outdir_root, "restarts.png"),
  plt,
  width = 18.5,
  height = 13,
  units = "in"
)

# zoomed <- plt +
#   xlim(
#     lubridate::ymd_h("2017-10-01T00"),
#     lubridate::ymd_h("2017-10-17T01")
#   )
# zoomed
# ggsave(
#   file.path(outdir_root, "zoomed.png"),
#   zoomed,
#   width = 14.5, height = 9, units = "in"
# )
