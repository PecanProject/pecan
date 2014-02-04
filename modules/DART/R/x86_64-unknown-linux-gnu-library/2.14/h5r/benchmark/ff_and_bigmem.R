require(bigmemory)
require(ff)
require(h5r)
require(pbutils)
require(RColorBrewer)

pdf("timing_results.pdf", width = 12, height = 8)

cols <- brewer.pal(3, "Set2")
reps <- 10
vsize <- 4:8
gDta <- function(s) as.integer(rpois(10^s, 1000))

creation <- lapply(vsize, function(s) {
  do.call(rbind, lapply(1:reps, function(r) {
    d <- gDta(s)
    system("rm -f /scratch/t.h5")
    h5 <- H5File("/scratch/t.h5", 'w')
    c("ff" = system.time(ff(d))[3],
      "bigmemory" = system.time(as.big.matrix(d))[3],
      "h5r" = system.time(createH5Dataset(h5, 'ds', d))[3])
  }))
})

local({
  par(mar=c(8, 6, 2, 2))
  m <- do.call(cbind, creation)
  boxplot(m, col = cols, las = 2, ylab = "Seconds", log = 'y', 
          main = paste("Creation times for vectors of 10^(", paste(vsize, collapse = ", "), ")", sep = ""),
          names = gsub("\\.elapse", "", colnames(m)), border = cols, pch = 16)
  legend("topleft", c("ff", "bigmemory", "h5r"), fill = cols)
})

insertion <- lapply(vsize, function(s) {
  do.call(rbind, lapply(1:reps, function(r) {
    d <- gDta(s)
    system("rm -f /scratch/t.h5")
    h5 <- H5File("/scratch/t.h5", 'w')
    
    c(
      "ff" = system.time({
        v <- ff(vmode = "integer", dim = length(d))
        v[] <- d
      })[3], 
      "bigmemory" = system.time({
        as.big.matrix(d)
      })[3],
      "h5r" = system.time({
        createH5Dataset(h5, 'ds', d)
      })[3]
      )
  }))
})

local({
  par(mar=c(8, 6, 2, 2))
  m <- do.call(cbind, insertion)
  boxplot(m, col = cols, las = 2, ylab = "Seconds", log = 'y', 
          main = paste("Creation+Insertion times for vectors of 10^(", paste(vsize, collapse = ", "), ")", sep = ""),
          names = gsub("\\.elapse", "", colnames(m)), border = cols, pch = 16)
  legend("topleft", c("ff", "bigmemory", "h5r"), fill = cols)
})


system("rm -f /scratch/t.h5")
h5 <- H5File("/scratch/t.h5", 'w')

D <- gDta(8)
hD <- createH5Dataset(h5, "dsq", D, chunkSizes = 2^16)
fD <- ff(D)
bD <- as.big.matrix(D)

selSize <- 10^(2:4)
selection <- lapply(selSize, function(s) {
  do.call(rbind, lapply(1:reps, function(r) {
    colSums(do.call(rbind, lapply(1:50, function(zz) {
      start <- round(runif(1, 1, length(D) - s - 10))
      end <- round(start + s)
      c(
        "ff" = system.time({
          fD[start:end]
        })[3], 
        "bigmemory" = system.time({
          bD[start:end, ]
        })[3],
        "h5r" = system.time({
          hD[start:end]
        })[3]
        )
    })))
  }))
})

local({
  par(mar=c(8, 6, 2, 2))
  m <- do.call(cbind, selection)
  boxplot(m, col = cols, las = 2, ylab = "Seconds", log = 'y',
          main = paste("Selection Times for 50 vectors of 10^(", paste(selSize, collapse = ", "), ")", sep = ""),
          names = gsub("\\.elapse", "", colnames(m)), border = cols, pch = 16)
  legend("topleft", c("ff", "bigmemory", "h5r"), fill = cols)
})

##
## vector selection
##
selSize <- 1000
nSelect <- 10^(2:5)

vselection <- lapply(nSelect, function(s) {
  do.call(rbind, lapply(1:reps, function(r) {
    start <- runif(s, 1, length(D) - 10*selSize)
    width <- rexp(s, 1/selSize)
    rngs <- round(cbind(start, width)) + 1

    print(range(rngs))
    c(
      "ff" = system.time({
        lapply(1:nrow(rngs), function(i) fD[rngs[i,1]:(rngs[i,1]+rngs[i,2])])
      })[3], 
      "bigmemory" = system.time({
        lapply(1:nrow(rngs), function(i) bD[rngs[i,1]:(rngs[i,1]+rngs[i,2]),])
      })[3],
      "h5r" = system.time({
        read1DSlabs(hD, rngs[,1], rngs[,2])
      })[3]
      )
  }))
})

local({
  par(mar=c(8, 6, 2, 2))
  m <- do.call(cbind, vselection)
  boxplot(m, col = cols, las = 2, ylab = "Seconds", log = 'y',
          main = paste("Selection Times for sets of ranges of length (", paste(nSelect, collapse = ", "), ")", sep = ""),
          names = gsub("\\.elapse", "", colnames(m)), border = cols, pch = 16)
  legend("topleft", c("ff", "bigmemory", "h5r"), fill = cols)
})

##
## random
##
selSize <- 10^(2:4)
rselection <- lapply(selSize, function(s) {
  do.call(rbind, lapply(1:reps, function(r) {
    colSums(do.call(rbind, lapply(1:10, function(zz) {
      points <- sample(1:length(D), size = s)
      c(
        "ff" = system.time({
          fD[points]
        })[3], 
        "bigmemory" = system.time({
          bD[points, ]
        })[3],
        "h5r" = system.time({
          hD[points]
        })[3]
        )
    })))
  }))
})

local({
  par(mar=c(8, 6, 2, 2))
  m <- do.call(cbind, rselection)
  m[m==0] <- min(m[m!=0])
  boxplot(m, col = cols, las = 2, ylab = "Seconds", log = 'y',
          main = paste("Selection Times for 10 random point vectors of size 10^(", paste(selSize, collapse = ", "), ")", sep = ""),
          names = gsub("\\.elapse", "", colnames(m)), border = cols, pch = 16)
  legend("topleft", c("ff", "bigmemory", "h5r"), fill = cols)
})

dev.off()
