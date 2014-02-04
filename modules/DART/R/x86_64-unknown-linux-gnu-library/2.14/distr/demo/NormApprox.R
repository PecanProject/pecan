require(distr)
options("newDevice"=TRUE)

N <- Norm(0,1)
U <- Unif(0,1)
U2 <- U + U 
U4 <- U2 + U2
U8 <- U4 + U4
U12 <- U4 + U8
NormApprox <- U12 - 6

x <- seq(-4,4,0.001)

opar <- par(mfrow = c(2,1), no.readonly = TRUE)

plot(x, d(NormApprox)(x),
     type = "l",
     xlab = "",
     ylab = "Density",
     main = "Exact and approximated density")
lines(x, d(N)(x),
      col = "red")
legend("topleft",
       legend = c("NormApprox", "Norm(0,1)"),
       fill = c("black", "red"))

plot(x, d(NormApprox)(x) - d(N)(x),
     type = "l",
     xlab = "",
     ylab = "\"black\" - \"red\"",
     col = "darkgreen",
     main = "Error")
lines(c(-4,4), c(0,0))

cat("Hit <enter> to continue...")
readline()


f.n <- function(z, n){
    sapply(X = z,
           FUN = function(z, n){
               ind <- 0:n
               sum((-1)^ind*choose(n, ind)*(z-ind)^(n-1)*(z > ind))/factorial(n-1)
           },
           n = n)
}

z <- seq(from = 0, to = 6, length = 1000)

par(mfrow = c(1,1))

plot(z, f.n(z, n=1), type = "l", ylab = expression(paste(f[n](z))), lwd = 2, col = 1)
lines(z, f.n(z, n=2), lwd = 2, col = 2)
lines(z, f.n(z, n=3), lwd = 2, col = 3)
lines(z, f.n(z, n=4), lwd = 2, col = 4)
lines(z, f.n(z, n=5), lwd = 2, col = 5)
lines(z, f.n(z, n=6), lwd = 2, col = 6)
lines(z, dnorm(z, mean = 3), col = "orange", lwd = 2)
legend("topleft", xjust = 1, legend = c(expression(paste(f[1])), expression(paste(f[2])),
                        expression(paste(f[3])), expression(paste(f[4])),
                        expression(paste(f[5])), expression(paste(f[6])), "N(0,1)"),
       fill = c(1:6, "orange"), ncol = 2)
title("Densitiy of the sum of uniform distributed RV's")

par(opar)
