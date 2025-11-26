library(dplyr)

average_by_trait <- function(group_df) {
  summarized_df <- group_df %>%
    group_by(TraitID) %>%
    summarise(
      mean_value = mean(mean_value, na.rm = TRUE),
      mean_sd    = mean(sd, na.rm = TRUE),
      mean_n     = mean(n, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(summarized_df)
}
