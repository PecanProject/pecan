test_that("The compare.summary function is correct", {
result <-as.data.frame(c(26.53369,26.53369,0.99,5,0.004,0.91433,0.02614),row.names=c("sd.f","sd.r","R","E","S","overlap","sd.overlap"))
set.seed(1)
a <- runif(100,1,100)
b <- a+5
expect_equal(round(compare.summary(a,b),5)[1:6,],result[1:6,])
})

