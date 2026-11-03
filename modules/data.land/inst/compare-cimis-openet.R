#!/usr/bin/env Rscript

if (interactive()) {
  devtools::load_all("modules/data.land")
} else {
  library(PEcAn.data.land)
}

design_points_path <- "~/projects/cimis-to-irrigation/design_points.csv"
cimis_eto_cog_path <- "~/data/CIMIS-ETo-COG"

dates <- seq.Date(as.Date("2020-03-01"), as.Date("2020-11-30"), "day")
design_points <- readr::read_csv(design_points_path) |>
  head(10)

cimis_et <- extract_cimis_dates(
  design_points,
  dates,
  cimis_eto_cog_path,
  .progress = TRUE
)

openet_et <- extract_openet_daily(
  design_points,
  min(dates),
  max(dates)
)

combined <- cimis_et |>
  dplyr::rename(cimis = etref_mm_day) |>
  dplyr::full_join(openet_et |> dplyr::rename(openet = et_mm_day))

combined_long <- combined |>
  tidyr::pivot_longer(c(cimis, openet), names_to = "source", values_to = "et_mm_day")

library(ggplot2)
ggplot(combined_long) +
  aes(x = date, y = et_mm_day, color = source) +
  geom_line() +
  facet_wrap(~id) +
  theme_bw()
