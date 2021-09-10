Obs.data.prepare.MultiSite <- function(obs.path="../Obs/LandTrendr_AGB_output50s.RData", site.ids) {
  load(obs.path)
  
  point_list$median_AGB[[1]] <- point_list$median_AGB[[1]] %>%
    filter(Site_ID != '1000000074')
  
  
  point_list$stdv_AGB[[1]] <- point_list$stdv_AGB[[1]] %>%
    filter(Site_ID != '1000000074')
  
  #--------------------------------------------------------------------------------
  #for multi site both mean and cov needs to be a list like this
  # +date
  #   +siteid
  #     c(state variables)/matrix(cov state variables)
  #
  #Filter all the obs just for the sites we are simulating
  point_list$median_AGB <-
    point_list$median_AGB[[1]] %>% filter(Site_ID %in% site.ids)
  point_list$stdv_AGB  <-
    point_list$stdv_AGB[[1]] %>% filter(Site_ID %in% site.ids)
  
  #Finding the orders
  site.order <-
    sapply(site.ids, function(x)
      which(point_list$median_AGB$Site_ID %in% x)) %>%
    as.numeric() %>% na.omit()
  
  #Reordering
  point_list$median_AGB <- point_list$median_AGB[site.order, ]
  point_list$stdv_AGB <- point_list$stdv_AGB[site.order, ]
  
  # truning lists to dfs  for both mean and cov
  date.obs <-
    strsplit(names(point_list$median_AGB), "_")[3:length(point_list$median_AGB)] %>%
    map_chr( ~ .x[2]) %>% paste0(., "/12/31")
  
  #Making in a format that we need
  obs.mean <-
    names(point_list$median_AGB)[3:length(point_list$median_AGB)] %>%
    map(function(namesl) {
      ((point_list$median_AGB)[[namesl]] %>%
         map( ~ .x %>% as.data.frame %>% `colnames<-`(c('AbvGrndWood'))) %>%
         setNames(site.ids[1:length(.)])
      )
    }) %>% setNames(date.obs)
  
  
  
  obs.cov <-
    names(point_list$stdv_AGB)[3:length(point_list$median_AGB)] %>%
    map(function(namesl) {
      ((point_list$stdv_AGB)[[namesl]] %>%
         map(~ (.x) ^ 2 %>% as.matrix()) %>%
         setNames(site.ids[1:length(.)]))
      
    }) %>% setNames(date.obs)
  
  
  
  return(list(obs.mean = obs.mean,
              obs.cov = obs.cov))
}