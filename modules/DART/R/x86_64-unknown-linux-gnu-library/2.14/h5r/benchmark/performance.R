##
## Investigate performance profiles of various access patterns.
##
require(h5r)

makeRanges <- function(d, n = 100, mu = 1000) {
  start <- runif(n, 1, length(d) - mu)
  end   <- start + round(rexp(n, 1/mu))
  end   <- ifelse(end > length(d), length(d), end)
  width <- end - start + 1
  cbind(start, width)
}

f <- function(d, ranges) {  
  g <- lapply(seq.int(1, nrow(ranges)), function(i) {
    readSlab(d, ranges[i,1], ranges[i,2])
  })
  return(TRUE)
}

g <- function(d, ranges) {  
  g <- read1DSlabs(d, ranges[,1], ranges[,2])
  return(TRUE)
}

h5Files <- lapply(list(unzipped = "/scratch/u_big.h5", zipped = "/scratch/z_big.h5"), H5File)
chunks <- c("1e3", "1e4", "1e5")
names(chunks) <- paste("cs", chunks, sep = "-")

l <- lapply(h5Files, function(h5) {
  a <- do.call(cbind, lapply(chunks, function(s) {
    d <- getH5Dataset(h5, paste("data", s, sep = "_"))
    do.call(rbind, lapply(1:20, function(i) {
      ranges <- makeRanges(d, n = 1000, mu = 10000)
      c(lapply = as.numeric(system.time(f(d, ranges))[3]),
        native = as.numeric(system.time(g(d, ranges))[3]))
    }))
  }))
  colnames(a) <- paste(rep(names(chunks), each = 2), colnames(a), sep = "_")
  return(a)
})
a <- do.call(cbind, l)
colnames(a) <- paste(rep(names(h5Files), each = 6), colnames(a), sep = "_")

par(mar=c(12, 5, 3, 1))
boxplot(a, las = 2, ylab = 'seconds', main = "Time for 1000 ranges of ~ 10000 long",
        log = 'y')


write.table(a, file = "rres.dta")


require(Genominator)

d <- data.frame( chr = 1, strand = 0, location = 1:100000000, value = rpois(100000000, 10))
expData <- importToExpData(d, "test.db", tablename = "big_d", overwrite = T)

r <- makeRanges(1:10000000, n = 100)
r <- data.frame(r)
r$chr <- 1
r$strand <- 0
r$end <- r$start+r$width
system.time(splitByAnnotation(expData, r, what = 'value'))
                           
