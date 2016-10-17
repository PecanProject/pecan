read.settings.db <- function(ids,bety){
  settings.list <- list()
  for(i in seq_along(ids)){
    settings.list[[i]] <- tbl(bety,"reference_runs") %>% filter(id == ids[i]) %>% 
      dplyr::select(settings) %>% collect() %>% unlist() %>%
      xmlToList(.,"pecan") %>% Settings()
    settings.list[[i]]$info <- list(reference_run_id = ids[i])
  }
  settings.multi <- MultiSettings(settings.list)
  # This may not be the best way to add database information back into the xml, but it's a start
  settings.multi$database[[bety$info$dbname]] <- bety$info[c("host","user","dbname")]
  return(settings.multi)
}
