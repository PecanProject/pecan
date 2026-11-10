# Manually enter site info
# Note met start and end are based on the formatted data at each site, which excludes NAs
US_EDN = list(id = "US_EDN",
              name = "Eden Landing",
              lat = 37.615,
              lon = -122.114,
              site.pft = "default",
              met.start = "2018-04-03",
              met.end = "2021-06-16")

US_SRR = list(id = "US_SRR",
              name = "Rush Ranch",
              lat = 38.200,
              lon = -122.026,
              site.pft = "default",
              met.start = "2014-03-12",
              met.end = "2018-09-20")

US_DMG = list(id = "US_DMG",
              name = "Dutch Slough",
              lat = 38.0015,
              lon = -121.6691,
              site.pft = "default",
              met.start = "2021-12-15",
              met.end = "2024-12-19") 

#Combine
site_info <- list(US_EDN,
                  US_SRR,
                  US_DMG) |>
  dplyr::bind_rows()

#Save
write.csv(site_info, 
          here::here("models", "peprmt", "demo_run", "data",
                     "site_info.csv"),
          row.names = F)
