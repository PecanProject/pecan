
start <- "2004-01-01 06:00:00"
end <- "2004-01-02 06:00:00"
x <- get.ncepmet(lat = 40, lon = -80, start.date = start, end.date = end)


test_that("get.ncepmet works",{

    expect_true(all(c("year", "day", "solarR", "Tmax", "Tmin", "Tavg", "RHmax", "RHmin", "RHavg", "WS", "precip") %in% colnames(x)))

    expect_equal(nrow(x), 1 + day(ymd_hms(end)) - day(ymd_hms(start)))
})
    
