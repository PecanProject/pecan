library(Rpreles)
data(hydata)
hydata[is.na(hydata)] <- -999
CO2 <- seq(376, 385, 1)
T=hydata[180:189, 'TAir']
fAPAR=seq(0, 1, len=10)
fAPAR[1] = 0; fAPAR[10]=1
Precip=hydata[180:189, 'Precip']

PAR=hydata[180:189, 'PPFD']
D=seq(0, 2, len=10)
DOY=1:10

defaults = c(413, 0.45, 0.118, 3, 0.748018, 13.23383, 
             -3.9657867, 18.76696, -0.130473, 0.034459, 0.450828, 
             2000, 0.4, 0.324463, 0.874151, 0.075601, 0.541605, 
             0.273584, 1.2, 0.33, 4.970496, 0, 0, 200, 0, 0, 20, 
             -999, -999, -999)
defaults[24:27] <- c(45,0,0,0)

o1 <- PRELES(PAR, T, D, Precip, CO2, fAPAR, DOY=DOY, p=defaults,
             returncols=c("GPP", "ET",  "fS", "fW", "SW",
                      "Canopywater", "SOG", "S"), LOGFLAG=0)

decid <- c(500, 0.45, 0.118, 3, 0.748018, 13.23383, 
                       -3.9657867, 18.76696, -0.130473, 0.034459, 0.450828, 
                       2000, 0.4, 0.324463, 0.874151, 0.075601, 0.541605, 
                       0.273584, 1.2, 0.33, 4.970496, 0, 0, 200, 0, 0, 0, 
                       -999, -999, -999)

decid[28:30] <- c(57, 1.5, 134)
DOY=200:209
o1 <- PRELES(PAR, T+10, D, Precip, CO2, fAPAR, DOY=DOY, p=decid,
               returncols=c("GPP", "ET",  "fS", "fW", "SW",
                      "Canopywater", "SOG", "S"), LOGFLAG=0)


