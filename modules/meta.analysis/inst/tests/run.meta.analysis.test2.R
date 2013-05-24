test_that("The run.meta.analysis function is correct", {
  result.distn <- c('gamma','beta','lnorm','weibull','gamma','gamma','norm','gamma','beta','beta','weibull','norm','lnorm','beta','weibull','weibull')
  result.parama <- c(1.47,1.50,-5.4,1.55,23.29,12.8,0.05,4.95,4.06,1.10,7.11,4.14,-3.90,2.00,1.37,3.69)
  result.paramb <- c(0.06,1.5,0.4,0.86,0.32,0.18,0.01,0.76,7.2,1.5,6.29,0.21,0.4,4,1.43,11.39)
  result.n <- c(0,0,0,47,19,50,74,36,0,0,16,0,0,0,363,967)
  tmp <- tempdir()
  example.dir <- system.file("data", package = "PEcAn.MA")
  
  file.copy(from=file.path(example.dir, "trait.data.RData"), to=file.path(tmp, "trait.data.RData"), overwrite=TRUE)
  file.copy(from=file.path(example.dir, "prior.distns.RData"), to=file.path(tmp, "prior.distns.RData"), overwrite=TRUE)
  
  
  settings <- list(outdir = tmp, 
                   pfts = list(pft = list(name = "ebifarm.pavi",outdir = tmp)),
                   meta.analysis = list(iter = 1000))
  run.meta.analysis()
  
  correct <- min(min(result.distn==post.distns$distn),min(result.parama == round(post.distns$parama,digits=2)),
                 min(result.paramb == round(post.distns$paramb,digits=2)), min(result.n==post.distns$n))
  
  expect_equal(correct,1)
})
