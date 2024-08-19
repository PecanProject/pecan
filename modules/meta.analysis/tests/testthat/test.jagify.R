
test_that("jagify correctly assigns treatment index of 1 to all control treatments, regardless of alphabetical order", {
  ## generate test data; controls assigned to early alphabet and late alphabet trt names
  testresult <- data.frame(citation_id = 1,
                           site_id = rep(1:2, each = 5),
                           name = rep(letters[1:5],2),
                           trt_id = as.character(rep(letters[1:5],2)),
                           control = c(1, rep(0,8), 1), 
                           greenhouse = c(rep(0,5), rep(1,5)),
                           date = 1,
                           time = NA,
                           cultivar_id = 1,
                           specie_id = 1,
                           n = 2,
                           mean = sqrt(1:10),
                           stat = 1,
                           statname = "SE",
                           treatment_id = 1:10
  )

  jagged.data <- jagify(testresult)
  expect_equal(jagged.data$trt_num[jagged.data$trt == "control"], c(1, 1))
})
