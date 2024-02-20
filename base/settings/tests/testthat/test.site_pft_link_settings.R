test_that("`site.pft.link.settings` able to link sites to pfts and update settings accordingly", {
  withr::with_tempfile("tf", fileext = ".csv", {
    settings <- list(
      host = "pecan",
      run = list(
        inputs = list(
          pft.site = list(
            path = tf
          )
        )
      )
    ) 
    df <- data.frame(
      site = c("1000025731", "1000025732"),
      pft = c("temperate.broadleaf.deciduous1", "temperate.needleleaf.evergreen")
    )
    write.csv(df, tf, row.names = FALSE)
    updated_settings <- site.pft.link.settings(settings)
    print(updated_settings)
    print(length(updated_settings$pfts))
    for(i in 1:length(updated_settings$pfts)) {
      expect_equal(updated_settings$pfts[[i]]$name, df$pft[i])
      expect_equal(updated_settings$pfts$pft$constants, 1)
    }
  })
})