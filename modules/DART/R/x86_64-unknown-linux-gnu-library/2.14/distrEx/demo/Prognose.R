require(distrEx) 
options("newDevice"=TRUE)

## by M. Kohl
## slightly modified by P.R. [for more recent versions of our packages]

###############################################################################
## Class: PrognCondition
## condition in case of the simple prognose model 
## y = x + u
## i.e., conditioning by realizations of y
###############################################################################

D1 <- PrognCondDistribution(Error = Td(1))
D2 <- PrognCondDistribution(Error = Td(3))
D3 <- PrognCondDistribution(Error = ConvexContamination(Norm(), Norm(4,1), size=0.1))
D4 <- PrognCondDistribution(Error = ConvexContamination(Norm(), Norm(0,9), size=0.1))
D5 <- PrognCondDistribution(Error = ConvexContamination(Norm(), Norm(0,9), size=0.2))

y <- seq(from = 0, to = 8, length = 100)
## posterior mean
f <- function(y, e1){ E(e1, fun = function(x){x[1]}, cond = y) }

## dauert etwas ...
system.time(erg1 <- sapply(y, f, D1)) # ca. 8 sec.
system.time(erg2 <- sapply(y, f, D2)) # ca. 8 sec.
system.time(erg3 <- sapply(y, f, D3)) # ca. 80 sec.
system.time(erg4 <- sapply(y, f, D4)) # ca. 80 sec.
system.time(erg5 <- sapply(y, f, D5)) # ca. 80 sec.

## posterior modus
post.mod <- function(cond, e1) {
    optimize(f = d(e1), interval = c(q(e1)(1e-3, cond), 
                        q(e1)(1e-3, cond, lower.tail = FALSE)), 
        tol = .Machine$double.eps^0.25, maximum = TRUE, cond = cond)$maximum
}

D0 <- PrognCondDistribution()

## takes some time
system.time(perg0 <- sapply(y, post.mod, D0)) # ca. 0.5 sec.
system.time(perg1 <- sapply(y, post.mod, D1)) # ca. 0.8 sec.
system.time(perg2 <- sapply(y, post.mod, D2)) # ca. 0.6 sec.
system.time(perg3 <- sapply(y, post.mod, D3)) # ca. 65 sec.
system.time(perg4 <- sapply(y, post.mod, D4)) # ca. 65 sec.
system.time(perg5 <- sapply(y, post.mod, D5)) # ca. 65 sec.

## plot
plot(y, 0.5*y, type = "l")
lines(y, erg1, col = "red")
lines(y, erg2, col = "blue")
lines(y, erg3, col = "orange")
lines(y, erg4, col = "darkgreen")
lines(y, erg5, col = "darkred")
title("Posterior Mean")

windows()
plot(y, perg0, type = "l", ylim = c(-0.5, 4))
lines(y, perg1, col = "red")
lines(y, perg2, col = "blue")
lines(y, perg3, col = "orange")
lines(y, perg4, col = "darkgreen")
lines(y, perg5, col = "darkred")
title("Posterior Modus")
