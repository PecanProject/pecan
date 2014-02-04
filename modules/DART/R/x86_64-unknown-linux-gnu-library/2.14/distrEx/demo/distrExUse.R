
####################################################################################
# example expectation operator
####################################################################################
require("distrEx")
options("newDevice"=TRUE)

D1 <- Norm(mean=2)
m1 <- E(D1)  # = 2
E(D1, function(x){ x^2 }) # E(D1^2)
# integrand with additional argument:
E(D1, function(x, m1){(x - m1)^2}, m1 = m1) # '$\Var$'
# same way
sd(D1);median(D1);mad(D1);IQR(D1)

## now same code but for Poisson:

D1 <- Pois(lambda=3)
m1 <- E(D1) # = 3
E(D1, function(x){ x^2 })
E(D1, function(x, m1){(x - m1)^2}, m1 = m1)
sd(D1);median(D1);mad(D1);IQR(D1)
