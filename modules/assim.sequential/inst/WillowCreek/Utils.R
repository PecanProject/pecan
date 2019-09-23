##'
##' Rounds a date to the previous 6 hours (0:00, 6:00, 12:00, or 18:00).
##' 
##' @author Luke Dramko
round.to.six.hours <-
  function(date = Sys.time() - lubridate::hours(2)) {
    if (is.character(date)) {
      date <- as.POSIXct(date, tz = "UTC")
    }
    forecast_hour = (lubridate::hour(date) %/% 6) * 6 #Integer division by 6 followed by re-multiplication acts like a "floor function" for multiples of 6
    forecast_hour = sprintf("%04d", forecast_hour * 100)
    date = as.POSIXct(
      paste0(
        lubridate::year(date),
        "-",
        lubridate::month(date),
        "-",
        lubridate::day(date),
        " ",
        substring(forecast_hour, 1, 2),
        ":00:00"
      ),
      tz = "UTC"
    )
    
    return(date)
  }


ploting_fluxes <- function(obs.raw) {
  obs.plot <- obs.raw %>%
    tidyr::gather(Param, Value,-c(Date)) %>%
    filter(!(
      Param %in% c(
        "FjDay",
        "U",
        "Day",
        "DoY",
        "FC",
        "FjFay",
        "Hour",
        "Month",
        "SC",
        "Ustar",
        "Year",
        "H",
        "Flag"
      )
    ),
    Value != -999) %>%
    #filter((Date %>% as.Date) %in% (names(prep.data) %>% as.Date())) %>%
    ggplot(aes(Date, Value)) +
    geom_line(aes(color = Param), lwd = 1) +
    geom_point(aes(color = Param), size = 3) +
    facet_wrap(~ Param, scales = "free", ncol = 1) +
    scale_x_datetime(
      breaks = scales::date_breaks("12 hour"),
      labels = scales::date_format("%m/%d-%H:00")
    ) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal(base_size = 15) +
    geom_hline(yintercept = 0)+
    labs(y = "") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust = 1))
  
  return(obs.plot)
}
