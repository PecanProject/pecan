##` PROSPECT Sensitivity Analysis
##'
##' Generates plots showing the contributions of PROSPECT parameters to
##' resulting spectra.

source("prospect.R")
library(reshape2)
library(ggplot2)

LENGTH <- 20

N.vec <- seq(1, 4, length=LENGTH)
Cab.vec <- seq(20, 80, length=LENGTH)
Cw.vec <- seq(0.005, 0.04, length=LENGTH)
Cm.vec <- seq(0.001, 0.02, length=LENGTH)

wl.vec <- 400:2500

ps.N <- data.frame(
        sapply(N.vec, function(x) prospect4(x, guess.inits["Cab"],
                                            guess.inits["Cw"],
                                            guess.inits["Cm"],
                                            n.a, cab.a, w.a, m.a)
        )
)
ps.N$Wavelength <- wl.vec
p.N <- melt(ps.N,
            value.name = "Reflectance",
            id.vars = "Wavelength",
            variable.name = "N")

p.N$N <- rep(N.vec, 1, each = length(wl.vec))
ggplot(p.N) + aes(x=Wavelength, y=Reflectance, color=N) + 
        geom_line() + 
        scale_color_gradient(low="darkred", high="green1", limits=c(1,4))


ps.Cab <- data.frame(
        sapply(Cab.vec, function(x) prospect4(guess.inits["N"],
                                                x,
                                            guess.inits["Cw"],
                                            guess.inits["Cm"],
                                            n.a, cab.a, w.a, m.a)
        )
)
ps.Cab$Wavelength <- wl.vec
p.Cab <- melt(ps.Cab,
            value.name = "Reflectance",
            id.vars = "Wavelength",
            variable.name = "Cab")

p.Cab$Cab <- rep(Cab.vec, 1, each = length(wl.vec))
ggplot(p.Cab) + aes(x=Wavelength, y=Reflectance, color=Cab) + 
        geom_line(aes(group=Cab)) + 
        scale_color_gradient(low="red", high="blue")


ps.Cw <- data.frame(
        sapply(Cw.vec, function(x) prospect4(guess.inits["N"],
                                              guess.inits["Cab"],
                                            x,
                                            guess.inits["Cm"],
                                            n.a, cab.a, w.a, m.a)
        )
)
ps.Cw$Wavelength <- wl.vec
p.Cw <- melt(ps.Cw,
            value.name = "Reflectance",
            id.vars = "Wavelength",
            variable.name = "Cw")

p.Cw$Cw <- rep(Cw.vec, 1, each = length(wl.vec))
ggplot(p.Cw) + aes(x=Wavelength, y=Reflectance, color=Cw) + 
        geom_line(aes(group=Cw)) + 
        scale_color_gradient(low="red", high="blue")


ps.Cm <- data.frame(
        sapply(Cm.vec, function(x) prospect4(guess.inits["N"],
                                              guess.inits["Cab"],
                                            guess.inits["Cw"],
                                            x,
                                            n.a, cab.a, w.a, m.a)
        )
)
ps.Cm$Wavelength <- wl.vec
p.Cm <- melt(ps.Cm,
            value.name = "Reflectance",
            id.vars = "Wavelength",
            variable.name = "Cm")

p.Cm$Cm <- rep(Cm.vec, 1, each = length(wl.vec))
ggplot(p.Cm) + aes(x=Wavelength, y=Reflectance, color=Cm) + 
        geom_line(aes(group=Cm)) + 
        scale_color_gradient(low="red", high="blue")
    
# p.Cab <- melt(ps.Cab,
#             value.name = "Reflectance",
#             id.vars = "Wavelength",
#             varnames = c("Wavelength", "Cab")) 
# 
# p.Cw <- melt(ps.Cw,
#             value.name = "Reflectance",
#             id.vars = "Wavelength",
#             varnames = c("Wavelength", "Cw")) 
# 
# p.Cm <- melt(ps.Cm,
#             value.name = "Reflectance",
#             id.vars = "Wavelength",
#             varnames = c("Wavelength", "Cm")) 
# 
# 

# plot.sens <- function(mat, TITLE, xlim=c(400,2500)) {
#   plot(wl.vec, mat[,1], type='l', col=1,
#        xlim=xlim,
#        ylim=c(0, max(mat) + 0.05),
#        xlab="Wavelength (nm)",
#        ylab="Reflectance")
#   for (i in 2:LENGTH){
#     lines(wl.vec, mat[,i], col=i)
#   }
#   title(TITLE)
# }
# 
# plot.sens(ps.N, sprintf("N: %.1f to %.1f", min(N.vec), max(N.vec)))
# plot.sens(ps.Cab, sprintf("Cab: %.0f to %.0f", min(Cab.vec), max(Cab.vec)))
# plot.sens(ps.Cw, sprintf("Cw: %.2f to %.2f", min(Cw.vec), max(Cw.vec)))
# plot.sens(ps.Cm, sprintf("Cm: %.4f to %.4f", min(Cm.vec), max(Cm.vec)))


