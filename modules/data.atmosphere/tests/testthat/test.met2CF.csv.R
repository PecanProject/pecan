context("testing csv import using met2CF.csv")

format <- list(orig  = c("Corrected.Date.Time", "Solar.Radiation..W.m2.", "Temp..C.",
                      "PAR_.umol_m.2_s.1.", "PAR_.mol_m.2_h.1.", "RH....", "Wind.Speed..m.s.", 
                      "VPD", "Rain..mm."),
               units = c("mdy_hm", "W m-2","celsius",
                         "umol m-2 s-1", "mol m-2 h-1", "%", "m s-1", "Pa","mm h-1"),
               bety  = c("datetime", "solar_radiation", "airT", 
                      "PAR", NA, "relative_humidity", "Wspd", NA, "precipitation_rate"),
               skip  = 0,
               unit.row = TRUE,
               na.strings = NA) 
# met2CF.csv(in.path = "data", in.file = "met2CF.csv.csv", outfolder = tempdir(), 
#            format = format,
#            lat = 42 + 47/60 + 30/6000,
#            lon = 76 + 7/60 + 20/6000)
