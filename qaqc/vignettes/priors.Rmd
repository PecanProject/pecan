%%%%%%%%%%%%%%%%%
  %% NOTES %%%%%%%
%% Required Files:
  %% glopnet.csv (can be downloaded using commented code)
%% skillman2008qyv.csv provided by Dr. John Skillman skillman@csusb.edu, from Skillman 2008
%% wullschleger1993updated.csv data from Wullschleger
%% wullschleger_join_usdaplants.csv plant functional types from USDA Plants db for species in Wullschleger 1998
%% xeri_windspeed.csv wind measurements observed at tower
%% priors.csv can be used if database connection not available
<<echo=FALSE,results=hide,label=settings>>=
  ### Settings and Functions
  library(PECAn)
library(DEoptim)
#options(error=recover)
set.seed(1)
iter <-10000 #jags chain and misc simulation length; 1k for test, 10k for real
prior.figures <- list()
prior.captions <- list()
##Define useful formatting functions


inits <- list(list(.RNG.seed = 1,
                   .RNG.name = "base::Mersenne-Twister"),
              list(.RNG.seed = 2,
                   .RNG.name = "base::Mersenne-Twister"),
              list(.RNG.seed = 3,
                   .RNG.name = "base::Mersenne-Twister"),
              list(.RNG.seed = 4,
                   .RNG.name = "base::Mersenne-Twister"))

@ 



\subsubsection*{Priors from data}
Priors were estimated by finding the best fit distribution to raw data sets include SLA and leaf turnover rate from the GLOPNET database (\cite{wright2004wwl}, $n=125, 40$ respectively), root turnover rate (\cite{gill2000gpr}, $n = 66$), and quantum yield (\cite{skillman2008qyv}, $n=56$).
Candidate distributions for these priors were Gamma, Weibull, log-Normal, and F because each of these traits is bound at zero.
In all cases we are interested in using the full distribution of across-species data as our prior constraint on what one individual species is capable of doing, as opposed to using the estimate of the mean of this distribution as our prior.

<<echo=FALSE,results=hide,label=glopnet>>=
  ## following GLOPNET instructions, need to download original dataset from web:
  ## library(gdata)
  ## glopnet.data <- read.xls("http://www.nature.com/nature/journal/v428/n6985/extref/nature02403-s2.xls", sheet = 1, pattern = 'BIOME', method = 'csv',  header = TRUE)[,c('BIOME', 'Species', 'GF', 'log.LL', 'log.LMA')]
  ## write.csv(data, 'glopnet.csv')
  
  glopnet.data <- read.csv('glopnet.csv') # Wright et. al. 2004 supplementary file 
glopnet.grass <- glopnet.data[which(glopnet.data$GF == 'G'), ] # GF = growth form; G=grass 

## turnover time (tt)
glopnet.grass$tt    <- 12/10^(glopnet.grass$log.LL)
## specific leaf area
##glopnet.grass$sla   <- 1000/ (0.48 * 10^glopnet.grass$log.LMA)
glopnet.grass$sla   <- 1000/ (10^glopnet.grass$log.LMA)
sladata <- data.frame(sla = glopnet.grass$sla[!is.na(glopnet.grass$sla)])
ttdata <- data.frame(tt = glopnet.grass$tt[!is.na(glopnet.grass$tt)])

dists <- c('gamma', 'lognormal','weibull', 'f')

prior.dists <- rbind('SLA' = fit.dist(sladata, dists ), 
                     'leaf_turnover_rate' = fit.dist(ttdata, dists))

slaprior <- with(prior.dists['SLA',], prior.density(distribution, a, b))
ttprior <- with(prior.dists['leaf_turnover_rate',], prior.density(distribution, a, b))

prior.figures[['SLA']]                <- priorfig(priordata = sladata, 
                                                  priordensity = slaprior, 
                                                  trait = 'SLA')
prior.figures[['leaf_turnover_rate']] <- priorfig(priordata = ttdata, 
                                                  priordensity = ttprior, 
                                                  trait = 'leaf_turnover_rate')
@ 

<<echo=FALSE,results=hide,label=rootturnover>>=
  
  ## data extracted from Gill and Jackson (2000) figure 3a
  rtt <- c(0.11, 0.14, 0.13, 0.29, 0.31, 0.06, 0.18, 0.038, 0.14, 0.21, 0.25, 0.32, 0.34, 0.32, 0.23, 0.21, 0.16, 0.22, 0.26, 0.31, 0.35, 0.3, 0.44, 0.47, 0.53, 0.55, 0.59, 0.49, 0.53, 0.74, 0.76, 0.94, 0.83, 0.67, 0.5, 0.47, 0.47, 0.41, 0.34, 0.34, 0.34, 0.5, 0.58, 0.66, 0.75, 0.82, 0.92, 0.88, 0.96, 0.81, 0.77, 0.76, 0.82, 0.83, 0.87, 0.72, 0.78, 0.72, 1.1, 1.1, 1.2, 1.2, 1.4, 1.6, 1.2, 1.5)
distfits <- list()

prior.dists <- rbind(prior.dists, 'root_turnover_rate' = fit.dist(data.frame(rtt=rtt), dists))
rttdensity <- with(prior.dists['root_turnover_rate',], prior.density(distribution, a, b))

prior.figures[['root_turnover_rate']] <- priorfig(priordata = data.frame(x = rtt), 
                                                  priordensity = rttdensity, 
                                                  trait = 'root_turnover_rate')

@

Quantum yield data represent a survey of published values of quantum yield in C4 monocots \citep{skillman2008qyv}; original data were provided by the author and restricted to measurements made  under photorespiratory conditions (ambient CO$_2$ and O$_2$)  (J. Skillman, personal communication).
Given the narrow range of data ($\text{CV} = 11\%$), the normal distribution was also considered but was not the best fit.
<<results=hide,echo=FALSE>>=
  skillman.data <- read.csv('skillman2008qyv.csv')
## as recommended by Dr. Skillman: data set restricted to data with photoresp.conditions. == 'y', i.e. collected using ambient gas (~0.03% CO2 and 21% O2)
qy.data   <- with(skillman.data, 
                  quantum.yield..absorbed.[pathway == 'C4' & 
                                             photoresp.conditions. == 'y' & 
                                             !is.na(quantum.yield..absorbed.)])

prior.dists <- rbind(prior.dists, 'quantum_efficiency' = fit.dist(qy.data, c(dists, 'normal')))
qy.density <- with(prior.dists['quantum_efficiency',], prior.density(distribution, a, b))
prior.figures[['quantum_efficiency']] <- priorfig(priordata = data.frame(qy.data),
                                                  priordensity = qy.density, 
                                                  trait = 'quantum_efficiency') 
@ 

\subsubsection*{Priors from meta-analysis}

We used meta-analysis to calculate a prior from data when summary statistics and sample sizes were available.
The meta-analysis model used to calculate prior distributions is similar to the one used by PEcAn to summarize species-level data (equation \ref{eq:mamodel}), with three differences.
First, there are no site, treatment, or greenhouse effects.
Second, data from multiple species were used.
Third, we generated a posterior predictive distribution to predict the distribution of trait values for an unobserved C4 plant species, unlike the species-level meta-analysis, which estimated the global mean parameter value.
Thus, the model included plant functional type (PFT) as a random effect: 
  
  \begin{equation}\label{eq:priormamodel}
\Theta_{\text{species}} = \beta{_0} + \beta_{\text{PFT}}
\end{equation}

Stomatal slope is the empirical slope coefficient in the \citep{leuning1995cac} model of stomatal conductance. 
Estimates of this parameter are sparse, so we collected new data for this study.
Stomatal slope was estimated using measurements of four leaves from each of five field-grown energy crop species during the 2010 growing season (Appendix A). 
The five species included two C4 grasses: Miscanthus (\textit{Miscanthus~x~giganteus}) and Switchgrass (\textit{P~virgatum}) planted in 2008 and three deciduous tree species: Red Maple (\textit{Acer~rubrum}), Eastern Cottonwood (\textit{Populus~deltoides}, and Sherburne Willow \textit{Salix~x~Sherburne}) planted in 2010 as 2 year old saplings. 
All plants were grown at the Energy Biosciences Institute Energy Farm (40$^o$10'N, 88$^o$03"W).
                                                                       We used the data from the three tree species and Miscanthus to calculate the posterior predictive distribution of an unobserved C4 grass species, and used this distribution as the prior estimate for Switchgrass stomatal slope.
                                                                       
                                                                       <<echo=FALSE,results=hide,label=stomatalslope>>=
                                                                       slope.data <- data.frame(Y=c(5.3, 3.4, 5.9, 6.6, NA),
                                                                       obs.prec = 1/sqrt(c(0.83, 0.21, 1.08, 0.57, NA)),
                                                                       ft = c(1, 2, 1, 1, 2),
                                                                       n = c(rep(4,4),1))
                                                                       
                                                                       writeLines(con = "stmslope.prior.bug",
                                                                       text = "model 
{
                                                                       for (k in 1:length(Y)) {
                                                                       Y[k] ~ dnorm(beta.ft[ft[k]], tau.y[k])T(0,)
                                                                       tau.y[k] <- prec.y * n[k]
                                                                       u1[k] <- n[k]/2
                                                                       u2[k] <- n[k]/(2 * prec.y)
                                                                       obs.prec[k] ~ dgamma(u1[k], u2[k])
                                                                       }
                                                                       for (g in 1:2) {
                                                                       beta.ft[g] ~ dnorm(0, 0.01)
                                                                       }  
                                                                       prec.y ~ dgamma(0.01, 0.01)
                                                                       sd.y  <- 1/sqrt(prec.y)
                                                                       pi.pavi <- Y[length(Y)]
}")
j.model  <- jags.model(file = "stmslope.prior.bug", 
                                                                       data = slope.data, 
                                                                       n.adapt = 500, 
                                                                       n.chains = 4,                       
                                                                       inits = inits)
                                                                       mcmc.object <- coda.samples(model = j.model, variable.names = c('pi.pavi','beta.ft'), n.iter = iter)
                                                                       mcmc.o <- window(mcmc.object, start = iter/2, thin = 10)
                                                                       
                                                                       pi.pavi    <- data.frame(stmslope = unlist(mcmc.o[,'pi.pavi']))
                                                                       stmslope.dist <- fit.dist(pi.pavi, n = 4)
                                                                       
                                                                       prior.dists   <- rbind(prior.dists, 'stomatal_slope' = stmslope.dist)
                                                                       stmslope.density <- with(stmslope.dist, prior.density(distribution, a, b), xlim = c(0,50))
                                                                       
                                                                       ######### Stomatal Slope Prior Plot
                                                                       stmslope.c4 <- slope.data$ft == 2 & !is.na(slope.data$Y)
                                                                       slope.data <- transform(slope.data, mean = Y, se =  1/(sqrt(n) * obs.prec))[,c('ft', 'mean', 'se')]
                                                                       
                                                                       prior.figures[['stomatal_slope']] <- 
                                                                       priorfig(priordensity = stmslope.density, 
                                                                       trait = 'stomatal_slope', 
                                                                       xlim = range(c(0, 8))) + 
                                                                       geom_point(data = subset(slope.data, ft != 2), 
                                                                       aes(x=mean, y = 2:4/60), color = 'grey60') +
                                                                       geom_segment(data = subset(slope.data, ft != 2),
                                                                       aes(x = mean - se, xend = mean + se, 
                                                                       y = 2:4/60, 
                                                                       yend = 2:4/60),
                                                                       color = 'grey60') +
                                                                       ## darker color for C4 grasses
                                                                       geom_point(data = subset(slope.data, ft == 2),
                                                                       aes(x=mean, y = 1/60)) +
                                                                       geom_segment(data = subset(slope.data, ft == 2),
                                                                       aes(x = mean - se, xend = mean + se, 
                                                                       y = 1/60, yend = 1/60))
                                                                       
                                                                       @ 
                                                                       
                                                                       
                                                                       
                                                                       Maximal carboxylation rate (V$_{\text{cmax}}$) data consists of ninety-four C3 species \citep{wullschleger1993blc} plus three C4 species \citep{kubien2005ltp,massad2007etc,wang2011icp}.
                                                                       To express V$_{\text{cmax}}$ at a common temperature of $25^oC$ for all species, we applied an Arrhenius temperature correction (Appendix C).
                                                                       The \citet{wullschleger1993blc} data set included a $95\%$ CI and an asymptotic error calculated by the SAS nlin proceedure (Stan Wullschleger, personal communication).
                                                                       We used the asymptotic error as an estimate of SE, the $95\%$ CI to estimate SD $\left(\text{SD} = \frac{\frac{1}{2}\text{CI}}{1.96}\right)$, and then estimated $n$ as $\hat{n}=\left(\frac{\text{SE}}{\text{SD}}\right)^2$.
                                                                       Plant species were classified into five functional types: C3 grass, C4 grass, forb, woody non-gymnosperm, and gymnosperm based on species records in the USDA PLANTS Database \citep{usda2011pd}.
                                                                       Ambiguous species (those with both forb and woody growth forms, $n = 15$) were excluded. 
                                                                       
                                                                       <<echo=FALSE,results=hide,label=wullschlegerdata>>=
                                                                       wullschleger.data <- read.csv('wullschleger1993updated.csv')
                                                                       
                                                                       
                                                                       
                                                                       ### Classify Wullschleger Data into Functional Types ###########
                                                                       ####### 1. query functional types from BETY
                                                                       ## wullschleger.species <- vecpaste(unique(wullschleger.data$AcceptedSymbol))
                                                                       ## ## load functional type data from BETY
                                                                       ## con <- function() {query.bety.con(username = "ebi_analys_user", password = "xxxxxxx", 
                                                                       ##                                   host = "ebi-forecast", dbname = "ebi_analysis")}
                                                                       ## functional.data <- query.bety(paste("select distinct AcceptedSymbol, scientificname, GrowthHabit, Category from species where AcceptedSymbol in (", wullschleger.species, ") and GrowthHabit is not NULL and Category is not NULL;"), con())
                                                                       ## write.csv(functional.data, 'wullschleger_join_usdaplants.csv')
                                                                       functional.data <- read.csv('wullschleger_join_usdaplants.csv')
                                                                       
                                                                       subshrubs <- rownames(wullschleger.data) %in% c(grep('Shrub',wullschleger.data$GrowthHabit), grep('Subshrub', wullschleger.data$GrowthHabit))
                                                                       ########## 2. Merge functional type information into Wullschleger data
                                                                       wullschleger.data <- merge(wullschleger.data, functional.data, by = 'AcceptedSymbol')
                                                                       ########## 3. Classify by functional type
                                                                       ## grass: any Graminoid Monocot
                                                                       grass <- with(wullschleger.data, GrowthHabit == 'Graminoid' & Category == 'Monocot')
                                                                       ## forbs/herbs = forb
                                                                       forb <- with(wullschleger.data, GrowthHabit == 'Forb/herb' | GrowthHabit == 'Vine, Forb/herb')
                                                                       ## woody = Shrubs, Subshrubs or Trees, add category to a few with missing information, 
                                                                       woody <- with(wullschleger.data, scientificname %in% c('Acacia ligulata', 'Acacia mangium', 'Arbutus unedo', 'Eucalyptus pauciflora', 'Malus', 'Salix') |  rownames(wullschleger.data) %in% c(grep('Shrub', GrowthHabit), grep('Subshrub', GrowthHabit), grep('Tree', GrowthHabit)))
                                                                       ## gymnosperms
                                                                       gymnosperm <- wullschleger.data$Category == 'Gymnosperm'
                                                                       ## ambiguous is both woody and herbaceous, will drop
                                                                       ambiguous <- wullschleger.data$GrowthHabit %in% c("Subshrub, Shrub, Forb/herb, Tree", "Tree, Subshrub, Shrub", "Tree, Shrub, Subshrub, Forb/herb", "Subshrub, Forb/herb")
                                                                       
                                                                       wullschleger.data$functional.type[grass] <- 1
                                                                       wullschleger.data$functional.type[forb]  <- 3
                                                                       wullschleger.data$functional.type[woody & !gymnosperm] <- 4
                                                                       wullschleger.data$functional.type[woody & gymnosperm] <- 5
                                                                       wullschleger.data <- subset(wullschleger.data, !ambiguous)     
                                                                       
                                                                       ############# Estimating SE and n ##################################
                                                                       ##verr, jerr: the "asymptotic" errors for Vcmax, Jmax using SAS nlim
                                                                       ##vucl,vlcl,jlcl,jucl: are upper and lower confidence limits of 95%CI
                                                                       
                                                                       ##Calculate SD as (1/2 the 95%CI)/1.96
                                                                       wullschleger.data$vsd <- (wullschleger.data$vucl-wullschleger.data$vlcl)/(2*1.96)
                                                                       
                                                                       ##Calculate effective N as (SE/SD)^2 + 1
                                                                       wullschleger.data$neff <- (wullschleger.data$vse/wullschleger.data$vsd)^2 + 1
                                                                       wullschleger.data$se   <- sqrt(wullschleger.data$vsd*sqrt(wullschleger.data$neff))
                                                                       ## recode species to numeric
                                                                       species <- unique(wullschleger.data$scientificname)
                                                                       sp <- rep(NA, nrow(wullschleger.data))
                                                                       for(i in seq(species)){
                                                                       sp[wullschleger.data$scientificname == species[i]] <- i
                                                                       } 
                                                                       
                                                                       ############# Scale values to 25C ##################################
                                                                       wullschleger.data$corrected.vcmax <- arrhenius.scaling(wullschleger.data$vcmax, 
                                                                       old.temp = wullschleger.data$temp, 
                                                                       new.temp = 25)
                                                                       
                                                                       ############# Create data.frame for JAGS model ##################################
                                                                       wullschleger.data <- data.frame(Y  = wullschleger.data$corrected.vcmax,
                                                                       obs.prec = 1 / (sqrt(wullschleger.data$neff) * wullschleger.data$se),
                                                                       sp = sp,
                                                                       ft = wullschleger.data$functional.type,
                                                                       n  = wullschleger.data$neff)
                                                                       ## Summarize data by species
                                                                       wullschleger.vcmax <- ddply(wullschleger.data, 
                                                                       .(sp),
                                                                       summarise, 
                                                                       Y = mean(Y), 
                                                                       obs.prec = 1/sqrt(sum((1/obs.prec)^2)), 
                                                                       ft = mean(ft), # identity
                                                                       n =  sum(n)) [,-1]
                                                                       
                                                                       
                                                                       ###### Add data from C4 species #########
                                                                       
                                                                       ####### Miscanthus: Wang D, Maughan MW, Sun J, Feng X, Miguez FE, Lee DK, Dietze MC, 2011. Impact of canopy nitrogen allocation on growth and photosynthesis of miscanthus (Miscanthus x giganteus). Oecologia, in press
                                                                       dwang.vcmax <- c(19.73, 40.35, 33.02, 21.28, 31.45, 27.83, 9.69, 15.99, 18.88, 11.45, 15.81, 27.61, 13, 21.25, 22.01, 10.37, 12.37, 22.8, 12.24, 15.85, 21.93, 23.48, 31.51, 23.16, 18.55, 17.06, 20.27, 30.41)
                                                                       dwang.vcmax <- data.frame(Y = mean(dwang.vcmax), 
                                                                       obs.prec = 1/sd(dwang.vcmax),
                                                                       ft = 2,         #C4 Grass
                                                                       n = length(dwang.vcmax))
                                                                       
                                                                       ###### Muhlenbergia glomerata: Kubien and Sage 2004. Low-temperature photosynthetic performance of a C4 grass and a co-occurring C3 grass native to high latitudes. Plant, Cell & Environment DOI: 10.1111/j.1365-3040.2004.01196.x
                                                                       ##   Data from figure 5, average across plants grown at 14/10 degrees and 26/22 degrees 
                                                                       ##   Species is Muhlenbergia glomerata 
                                                                       kubien.vcmax <- data.frame(Y = mean(23.4, 24.8), obs.prec = 1/sqrt(2.6^2 + 2.5^2), n = 8, ft = 2)                      
                                                                       ###### Zea Mays (Corn): Massad, Tuzet, Bethenod 2007. The effect of temperature on C4-type leaf photosynthesis parameters. Plant, Cell & Environment 30(9) 1191-1204. DOI: 10.1111/j.1365-3040.2007.01691.x
                                                                       ## data from fig 6a
                                                                       massad.vcmax.data <- data.frame(vcmax = c(17.1, 17.2, 17.6, 18, 18.3, 18.5, 20.4, 22.9, 22.9, 21.9, 21.8, 22.7, 22.3, 25.3, 24.4, 25.5, 25.5, 24.9, 31.2, 30.8, 31.6, 31.7, 32.5, 34.1, 34.2, 33.9, 35.4, 36, 36, 37.5, 38.2, 38.1, 37.4, 37.7, 25.2, 25.5), 
                                                                       temp = c(20.5, 24.6, 21, 19, 21.9, 15, 19.6, 14.3, 20.8, 23.4, 24.8, 25.9, 24.1, 22.8, 27.9, 31.7, 35.5, 39.3, 37.9, 42.4, 41.5, 48.7, 33.3, 33.3, 31.5, 39.1, 38.8, 43.3, 50, 38.4, 35.7, 34.4, 31.9, 33.9, 32.1, 33.7))
                                                                       massad.vcmax <- with(massad.vcmax.data, arrhenius.scaling(old.temp = temp, 
                                                                       new.temp = 25, 
                                                                       observed.value = vcmax))
                                                                       
                                                                       massad.vcmax <- data.frame(Y  = mean(massad.vcmax),
                                                                       obs.prec = 1/(sd(massad.vcmax)),
                                                                       n  = length(massad.vcmax),
                                                                       ft = 2)
                                                                       
                                                                       ##### Combine all data sets
                                                                       all.vcmax.data <- rbind(wullschleger.vcmax,
                                                                       dwang.vcmax, 
                                                                       ## xfeng.vcmax, 
                                                                       kubien.vcmax, 
                                                                       massad.vcmax)
                                                                       ##### take a look at the raw data by functional type:
                                                                       # qplot(data = all.vcmax.data, x = factor(ft), y = Y, geom = 'boxplot')
                                                                       
                                                                       ##### Add unobserved C4 species for posterior predictive
                                                                       vcmax.data <- rbind(all.vcmax.data, 
                                                                       data.frame(Y = NA, obs.prec = NA, ft = 2, n = 1))
                                                                       
                                                                       writeLines(con = "vcmax.prior.bug", 
                                                                       text =  "model{	
                                                                       for (k in 1:length(Y)){
                                                                       Y[k]  ~ dnorm(beta.ft[ft[k]], tau.y[k])T(0,)
                                                                       tau.y[k] <- prec.y*n[k]
                                                                       u1[k] <- n[k]/2                             
                                                                       u2[k] <- n[k]/(2*prec.y)
                                                                       obs.prec[k] ~ dgamma(u1[k], u2[k])
                                                                       }
                                                                       for (f in 1:5){
                                                                       beta.ft[f] ~ dnorm(0, tau.ft)
                                                                       }
                                                                       tau.ft ~ dgamma(0.1, 0.1)
                                                                       prec.y    ~ dgamma(0.1, 0.1)     
                                                                       sd.y      <- 1 / sqrt(prec.y)
                                                                       ## estimating posterior predictive for new C4 species
                                                                       pi.pavi <- Y[length(Y)]	     
                                                                       diff <- beta.ft[1] - beta.ft[2]
                                                                       }")

                                                                       j.model  <- jags.model(file = "vcmax.prior.bug", 
                                                                       data = vcmax.data, 
                                                                       n.adapt = 500, 
                                                                       n.chains = 4,
                                                                       inits = inits)
                                                                       
                                                                       mcmc.object <- coda.samples(model = j.model, variable.names = c('pi.pavi', 'beta.ft', 'diff'),
                                                                       n.iter = iter)
                                                                       mcmc.o     <- window(mcmc.object, start = iter/2, thin = 10)
                                                                       pi.pavi    <- data.frame(vcmax = unlist(mcmc.o[,'pi.pavi']))
                                                                       vcmax.dist <- fit.dist(pi.pavi, n = sum(!is.na(vcmax.data$Y)))
                                                                       
                                                                       prior.dists   <- rbind(prior.dists, 'Vcmax' = vcmax.dist)
                                                                       vcmax.density <- with(vcmax.dist, prior.density(distribution, a, b), xlim = c(0,50))
                                                                       
                                                                       ######### Vcmax Prior Plot
                                                                       vcmax.c4 <- all.vcmax.data$ft == 2
                                                                       vcmax.data <- transform(all.vcmax.data, mean = Y, se =  1/(sqrt(n) * obs.prec))[,c('ft', 'mean', 'se')]
                                                                       #vcmax.mean <- all.vcmax.data$Y[!c4,]
                                                                       #vcmax.se   <- with(all.vcmax.data[!c4,], 1/(sqrt(n) * obs.prec))#[reorder]
                                                                       #c4.mean <- all.vcmax.data$Y)[c4]
                                                                       #c4.se   <- with(all.vcmax.data, 1 / (sqrt(n) * obs.prec) )[c4]
                                                                       
                                                                       prior.figures[['Vcmax']] <- 
                                                                       priorfig(priordensity = vcmax.density, 
                                                                       trait = 'Vcmax', 
                                                                       xlim = range(c(0, max(vcmax.data$mean)))) + 
                                                                       geom_point(data = subset(vcmax.data, ft != 2), 
                                                                       aes(x=mean, y = (sum(vcmax.c4) + 1:sum(!vcmax.c4))/3000), color = 'grey60') +
                                                                       geom_segment(data = subset(vcmax.data, ft != 2),
                                                                       aes(x = mean - se, xend = mean + se, 
                                                                       y = (sum(vcmax.c4) + 1:sum(!vcmax.c4))/3000, 
                                                                       yend = (sum(vcmax.c4) + 1:sum(!vcmax.c4))/3000),
                                                                       color = 'grey60') +
                                                                       ## darker color for C4 grasses
                                                                       geom_point(data = subset(vcmax.data, ft == 2),
                                                                       aes(x=mean, y = 1:sum(vcmax.c4)/2000), size = 3) +
                                                                       geom_segment(data = subset(vcmax.data, ft == 2),
                                                                       aes(x = mean - se, xend = mean + se, 
                                                                       y = 1:sum(vcmax.c4)/2000, yend = 1:sum(vcmax.c4)/2000))
                                                                       
                                                                       @ 
                                                                       
                                                                       Leaf width data represent 18 grass species  grown in a common garden greenhouse experiment \citep{oyarzabal2008tdb}.
                                                                       \emph{P. virgatum} was among the 18 species, and was excluded from the prior estimation but used as raw data in the meta-analysis.
                                                                       The remaining seventeen species were divided into C3 and C4 functional types. 
                                                                       Relative to the small sample of C4 grasses, switchgrass leaf width was an outlier;  inflating the variance four-fold reduced the prior information content to account for this descrepency. 
                                                                       
                                                                       %% for gamma: mean = shape/rate, variance = shape/rate^2 
                                                                       <<echo=FALSE,results=hide,label=leafwidth>>=
                                                                       ## insert leaf_width data from Oyarzabal 2008
                                                                       ## from appendix 1 (oryzabal2008tdb_appendix1.pdf)                 
                                                                       ## Switchgrass data not included
                                                                       leaf.width <- c(0.44, 0.46, 0.52, 0.2, 0.37, 0.16, 0.20, 0.28, 0.37, 0.32, 0.81, 0.09, 0.43, 0.22, 0.02, 0.04, 0.37)*10
                                                                       leaf.width.se <- c(0.05, 0.05, 0.12, 0.001, 0.05, 0.04, 0.001, 0.07, 0.08, 0.04, 0.31, 0.007, 0.05, 0.04, 0.001, 0.004, 0.04)*10
                                                                       leaf.width.n <- c(7, 5, 6, 6, 7, 6, 6, 7, 7, 6, 8, 7, 7, 7, 6, 5, 6)
                                                                       leaf.width.sd <- leaf.width.se*sqrt(leaf.width.n)
                                                                       lw.ft <- c(1, 2, 2, 2, 1, 2, 1, 1, 2, 1, 2, 1, 2, 1, 1, 1, 1) ## c3 = 1, c4 = 2
                                                                       
                                                                       lw.data <- data.frame('Y' = leaf.width, 'n' = leaf.width.n,  'obs.prec' = 1/leaf.width.sd, ft = lw.ft)
                                                                       
                                                                       lw.data <- rbind(lw.data, c(NA, 1, NA, 2))
                                                                       
                                                                       writeLines(con = "lw.prior.bug", 
                                                                       text =  "model 
{
                                                                       for (k in 1:length(Y)) {
                                                                       Y[k] ~ dnorm(Z[k], tau.y[k])
                                                                       Z[k] <- beta.ft[ft[k]]
                                                                       tau.y[k] <- prec.y * n[k]
                                                                       u1[k] <- n[k]/2
                                                                       u2[k] <- n[k]/(2 * prec.y)
                                                                       obs.prec[k] ~ dgamma(u1[k], u2[k])
                                                                       }
                                                                       for (g in 1:2) {
                                                                       beta.ft[g] ~ dnorm(0, 0.001)
                                                                       }	
                                                                       prec.y ~ dgamma(0.001, 0.001)
                                                                       sd.y <- 1/sqrt(prec.y)
                                                                       pi.pavi <- Y[length(Y)]
                                                                       diff <- beta.ft[1] - beta.ft[2]
}")
           
                                                                       j.model  <- jags.model(file = 'lw.prior.bug',
                                                                       data = lw.data, 
                                                                       n.adapt = iter / 10,
                                                                       n.chains = 4,
                                                                       inits = inits)
                                                                       
                                                                       mcmc.object <- coda.samples(model = j.model, 
                                                                       variable.names =  c('pi.pavi', 'beta.ft', 'diff'),
                                                                       n.iter = iter) 
                                                                       
                                                                       mcmc.o <- window(mcmc.object, start = iter / 2, thin = 50)
                                                                       
                                                                       pi.pavi <- unlist(mcmc.o[,'pi.pavi'])
                                                                       lw.dist <- fit.dist(pi.pavi, 
                                                                       dists = c('gamma', 'weibull', 'lognormal', 'normal'), 
                                                                       n = 18)
                                                                       
                                                                       prior.dists <- rbind(prior.dists, 'leaf_width' = lw.dist)
                                                                       
                                                                       
                                                                       ####### Plot Prior
                                                                       leaf.width.density <- do.call('prior.density', lw.dist[1:3])
                                                                       
                                                                       c4 <- lw.ft == 2
                                                                       c3.mean <- leaf.width[!c4]
                                                                       c3.se <- leaf.width.se[!c4]
                                                                       c4.mean <- leaf.width[c4]
                                                                       c4.se <- leaf.width.se[c4]
                                                                       
                                                                       prior.figures[['leaf_width']] <- priorfig(priordensity = leaf.width.density, 
                                                                       trait = 'leaf_width',
                                                                       xlim = c(0,10)) + 
                                                                       geom_point(aes(x=c3.mean, y = 8:17/100), color = 'grey') +
                                                                       geom_segment(aes(x = c3.mean - c3.se, 
                                                                       xend = c3.mean + c3.se, 
                                                                       y = 8:17/100, yend = 8:17/100), color = 'grey' ) +
                                                                       geom_point(aes(x=c4.mean, y = 1:6/100), size = 3) +
                                                                       geom_segment(aes(x = c4.mean - c4.se, 
                                                                       xend = c4.mean + c4.se, 
                                                                       y = 1:6/100, yend = 1:6/100))
                                                                       
                                                                       @ 
                                                                       
                                                                       
                                                                       Root respiration rate values were measured on thirty-six plants representing five functional types, including six C4 grass species \citep{tjoelker2005llr}.
                                                                       As before, \emph{P. virgatum} data was excluded from the prior estimation and used as raw data in the species-level meta-analysis. 
                                                                       
                                                                       <<echo=FALSE,results=hide,label=rootresp.data>>=
                                                                       
                                                                       ##### Data
                                                                       mean <- c(11.1, 13.7, 13.6, 11.7, 7.7, 4.7, 7.2, 5.2, 5.3, 5.0, 
                                                                       7.4, 16.5, 17.2, 23.3, 15.3, 19.2, 18.9, 9.8, 14.3, 7.1,
                                                                       9.9, 13.5, 9.7, 3.2, 19.2, 16.1, 9.5, 9.5, 16.4, 9.6, 
                                                                       22.4, 17.6, 11.3, 10.0, 6.8, 6.7)
                                                                       se   <- c(1.1, 2.0, 2.3, 1.1, 0.8, 0.6, 1.2, 0.7, 0.8, 0.6, 
                                                                       0.4, 1.3, 0.4, 3.1, 1.1, 1.1, 1.1, 2.1, 2.1, 4.5, 
                                                                       1.9, 0.9, 2.3, 2.1, 6.1, 0.4, 1.1, 1.7, 1.8, 1.4, 
                                                                       3.5, 3.8, 2.6, 3.2, 0.8, 0.2)
                                                                       n    <- c(4, 4, 4, 3, 4, 4, 4, 4, 3, 4, 
                                                                       3, 4, 3, 4, 4, 3, 4, 2, 4, 2, 
                                                                       4, 4, 4, 2, 3, 4, 4, 3, 4, 4, 
                                                                       3, 3, 4, 3, 4, 4)
                                                                       functional.type <- c(rep(1, 5), rep(2, 6), rep(3, 17), rep(4, 6), rep(5, 2)) 
                                                                       ## 1 = c3, 2 = c4, 3 = forb, 4 = legume, 5 = woody
                                                                       species <- c('Agropyron repens', 'Agrostis scabra', 'Koeleria cristata', 'Poa pratensis', 'Stipa spartea', 'Andropogon gerardii', 'Bouteloua curtipendula', 'Calamovifa longifolia', 'Panicum virgatum', 'Schizachyrium scoparium', 'Sorghastrum nutans', 'Achillea millefolium', 'Agastache foeniculum', 'Ambrosia artemisifolia', 'Anemone cylindrica', 'Asclepias syriaca', 'Asclepias tuberosa', 'Asclepias verticillata', 'Aster azureus', 'Aster ericoides', 'Coreopsis palmata', 'Liatras aspera', 'Penstemon grandiflorus', 'Potentilla arguta', 'Rudbeckia serotina', 'Solidago nemoralis', 'Solidago rigida', 'Solidago speciosa', 'Desmodium canadense', 'Lespedeza capitata', 'Lupinus perennis', 'Petalostemon candidum',  'Petalostemon purpureum',  'Petalostemon villosum', 'Corylus americana', 'Quercus macrocarpa')
                                                                       
                                                                       pavi.data <- data.frame(Y=mean, n=n, 
                                                                       obs.prec = 1/(se * sqrt(n)), 
                                                                       ft = functional.type)[!species == "Panicum virgatum", ]
                                                                       pavi.data <- rbind(pavi.data, c(Y=NA, n=1, obs.prec = NA, ft = 2)) 
                                                                       
                                                                       writeLines(con = "rootresp.prior.bug",
                                                                       text = "model 
{
                                                                       for (k in 1:length(Y)) {
                                                                       Y[k] ~ dnorm(beta.ft[ft[k]], tau.y[k])T(0,)
                                                                       tau.y[k] <- prec.y * n[k]
                                                                       u1[k] <- n[k]/2
                                                                       u2[k] <- n[k]/(2 * prec.y)
                                                                       obs.prec[k] ~ dgamma(u1[k], u2[k])
                                                                       }
                                                                       for (g in 1:5) {
                                                                       beta.ft[g] ~ dnorm(0, 0.01)
                                                                       }	
                                                                       prec.y ~ dgamma(0.01, 0.01)
                                                                       sd.y  <- 1/sqrt(prec.y)
                                                                       pi.pavi <- Y[length(Y)]
}")

                                                                       j.model  <- jags.model(file = "rootresp.prior.bug", 
                                                                       data = pavi.data, 
                                                                       n.adapt = 500, 
                                                                       n.chains = 4,                       
                                                                       inits = inits)
                                                                       mcmc.object <- coda.samples(model = j.model, variable.names = c('pi.pavi','beta.ft'), n.iter = iter)
                                                                       mcmc.o <- window(mcmc.object, start = iter/2, thin = 10)
                                                                       
                                                                       pi.pavi <- data.frame(rrr = unlist(mcmc.o[,'pi.pavi']))
                                                                       
                                                                       ## Convert from measurement temp 26C to BETYdb reference temp 25
                                                                       pi.pavi$rrr <- arrhenius.scaling(old.temp = 26, new.temp = 25, observed.value = pi.pavi$rrr)
                                                                       
                                                                       root.resp.dist <- fit.dist(pi.pavi, trait = 'rrr', dists = c('gamma', 'lognormal', 'weibull', 'normal'), n = 35)
                                                                       prior.dists <- rbind(prior.dists, 'root_respiration_rate' = root.resp.dist) 
                                                                       
                                                                       ##### Plot Prior
                                                                       root.resp.density <- prior.density(root.resp.dist$distribution, tabnum(root.resp.dist$a), tabnum(root.resp.dist$b))
                                                                       
                                                                       c4 <- functional.type == 2
                                                                       c4.mean <- mean[c4]
                                                                       c4.se   <- se[c4]
                                                                       c3.mean <- mean[!c4]
                                                                       c3.se   <-  se[!c4]
                                                                       prior.figures[['root_respiration_rate']] <- priorfig(priordensity = root.resp.density, 
                                                                       trait = 'root_respiration_rate',
                                                                       xlim = c(0, max(mean + se))) +
                                                                       geom_point(aes(x=c3.mean, y = (length(c4.mean)+1:length(c3.mean))/400), color = 'grey60') +
                                                                       geom_segment(aes(x = c3.mean - c3.se, xend = c3.mean + c3.se, y = (length(c4.mean)+1:length(c3.mean))/400, yend = (length(c4.mean)+1:length(c3.mean))/400), color = 'grey60' ) +
                                                                       geom_point(aes(x=c4.mean, y = (1:length(c4.mean)/400)), size = 3) +
                                                                       geom_segment(aes(x = c4.mean - c4.se, xend = c4.mean + c4.se, y = 1:length(c4.mean)/400, yend = 1:length(c4.mean)/400))
                                                                       
                                                                       @ 
                                                                       
                                                                       \subsubsection*{Priors from limited data and expert knowledge}
                                                                       
                                                                       When available data were limited to a few observations, these were used to identify a central tendency such as the mean, median, or mode, while expert knowledge was used to estimate the range of a confidence interval.
                                                                       An optimization approach was used to fit a probability distribution to this combination of data and expert constraint.
                                                                       
                                                                       In order to estimate the fine root to leaf ratio for  grasses, we assume fine roots account for all belowground biomass \citep{jackson1997gbf} and that leaves account for all above ground biomass.
                                                                       Roots account for approximately $2/3$ of total biomass across temperate grassland biomes  \citep[Table 23.1]{saugier2001egt}, so we constrained a beta prior on the root fraction to have a mean of $2/3$ by setting $\alpha = \beta/2$ since the mean of a beta is defined as $\frac{\alpha}{\alpha + \beta}$.
                                                                       To convert from proportion to ratio, we used the identity: if $X\sim\operatorname{Beta}(\frac{\alpha}{2},\frac{\beta}{2})$ then $\frac{X}{1-X} \sim \operatorname{F}(\alpha, \beta)\times\frac{\alpha}{\beta}$.
                                                                       We constrained the $95\%CI=\left[^1/_3,^{10}/_{11}\right]$, equivalent to a fine root to leaf ratio with a mean fixed at two and a $95\%\text{CI} = \left[^1/_2, 10\right]$.
                                                                       We simulated the distribution of the fine root to leaf ratio by drawing $\Sexpr{iter}$ samples from a $\operatorname{F}(2\alpha, \alpha)$ distribution and multiplying these samples by two.
                                                                       <<label=q,results=hide,echo=FALSE>>=
                                                                       
                                                                       parm <- DEoptim(fn = prior.fn, 
                                                                       lower = c(1), 
                                                                       upper = c(10), 
                                                                       x=c(1/3, 10/11, 2/3), 
                                                                       alpha = 0.05, 
                                                                       distn = 'beta', 
                                                                       central.tendency = 'mean', 
                                                                       trait = 'fineroot2leaf')$optim$bestmem
                                                                       
                                                                       set.seed(10)
                                                                       ### sample from F(2*alpha, beta)*2
                                                                       root2shoot <- rf(iter, 2*parm, parm)*2
                                                                       ### fit to sample
                                                                       root2shoot.dist <- fit.dist(root2shoot, trait = 'fineroot2leaf', 
                                                                       dists = c('lognormal', 'weibull', 'gamma', 'normal'), 
                                                                       n = 0)
                                                                       prior.dists <- rbind(prior.dists, 
                                                                       'fineroot2leaf' = root2shoot.dist)
                                                                       
                                                                       ##### Prior Plot
                                                                       root2shoot.density <- do.call('prior.density', root2shoot.dist[,c('distribution', 'a', 'b')])
                                                                       
                                                                       prior.figures[['fineroot2leaf']] <- priorfig(priordensity = root2shoot.density, trait = 'fineroot2leaf', xlim = c(0,15)) +
                                                                       geom_vline(aes(xintercept = c(0.5, 2, 10),  linetype = c(2,1,2)), color = 'grey')
                                                                       @
                                                                       
                                                                       Seed dispersal in ED2 represents the proportion of seed dispersed outside of a  7.5m radius plot, which we approximate as a beta distribution.
                                                                       Although no direct measurements of seed dispersal were available, it was straightforward to parametrize a ballistic model of seed dispersal (\cite{ernst1992pda}, from Creemer 1977): $D=\frac{V_wH}{V_t}$.
                                                                       This model relates dispersal distance $D$ to terminal velocity $V_t$, wind speed $V_w$, and seed height $H$.
                                                                       Although more sophisticated treatments of dispersal exist and are important for estimating low probability long distance dispersal events \citep{clark1999sdn,thompson2008ppf}, the \citet{ernst1992pda} model is sufficient for relatively short dispersal distances required in the present context.
                                                                       
                                                                       Values of terminal velocity, $V_t$, of grass seeds were taken from two references, \citep{ernst1992pda, jongejans1999msd} 
                                                                       <<echo=FALSE,results=hide>>=
                                                                       jongejans <-  c(0.83, 2.62, 4.35, 0.43, 0.84, 0.92, 0.98, 1.04, 0.33, 1.53, 1.62, 1.86, 2.01, 2.34, 2.71, 2.72, 3.41, 3.62, 4.13)
                                                                       ernst     <-  c(2.16, 1.24, 0.9, 0.61, 1.08, 1.19, 1.45, 2.44, 1.34, 2.22, 1.57) 
                                                                       vt.data <- data.frame(vt = c(jongejans, ernst))
                                                                       
                                                                       vt.prior <- fit.dist(vt.data)
                                                                       @  
                                                                       and these data were best described as $V_t\sim \operatorname{Gamma}(\Sexpr{tabnum(vt.prior$a)}, \Sexpr{tabnum(vt.prior$b)})$.
                                                                       
                                                                       Next the heights from which the seeds are dropped was estimated from calibrated photographs of reproductively mature switchgrass at a field site in Urbana, IL: $H\sim \operatorname{N}(2,0.5)$. 
                                                                       Finally, wind speed observed at the site were fit to a Weibull distribution \citep{justus1978mew}.
                                                                       <<echo=FALSE,results=hide>>=
                                                                       
                                                                       ## Extracted from Marcelo Xeri / Carl Bernacchi unpublished flux tower data
                                                                       ## wind_speed <- read.csv('FluxSW_EC_PV_2009.csv')[,c('WindSpeed_Avg')]
                                                                       ## write.csv(wind_speed, 'xeri_windspeed.csv', row.names = FALSE)
                                                                       wind_speed <- read.csv('xeri_windspeed.csv')$x
                                                                       vw.prior   <- fit.dist(wind_speed, 'weibull')
                                                                       @ 
                                                                       $V_w\sim \operatorname{Weibull}(\Sexpr{tabnum(vw.prior$a)},\Sexpr{tabnum(vw.prior$b)})$  (Marcelo Zeri, unpublished wind and height data).
                                                                       Given these distributions of $V_w$, $H$, and $V_t$, sets of 100 dispersal distances were simulated \Sexpr{iter} times to calculate the fraction of seeds in each simulation dispersed beyond 7.5m,
                                                                       <<echo=FALSE,results=hide>>=
                                                                       nld <- vector(length = iter)
                                                                       for(i in 1:iter) {
                                                                       Vw  <- pr.samp(vw.prior$distribution, vw.prior$a, vw.prior$b, 100) # sample wind velocities
                                                                       H   <- rnorm(100, 2, 0.5) # sample seed heights
                                                                       Vt  <- pr.samp(vt.prior$distribution, vt.prior$a, vt.prior$b, 100) # sample seed velocity
                                                                       D <- Vw*H/Vt # calculate dispersal distances
                                                                       a <- ecdf(D) # empirical cumulative distribution function of D
                                                                       nld[i] <- 1-a(7.5) # fraction of seeds dispersed beyond 7.5m
                                                                       }
                                                                       
                                                                       nld.sim   <- data.frame(nld = nld)
                                                                       nld.prior <- fit.dist(nld.sim, dists = 'beta', n = 30) #30 term. vel. observations
                                                                       
                                                                       prior.dists <- rbind(prior.dists, 
                                                                       'nonlocal_dispersal' = nld.prior)
                                                                       
                                                                       nld.dens <- prior.density(distribution = nld.prior$distribution, a = nld.prior$a, b = nld.prior$b)
                                                                       prior.figures[['nonlocal_dispersal']] <- priorfig(priordensity = nld.dens, trait = 'nonlocal_dispersal', xlim = c(0, 0.5))
                                                                       
                                                                       
                                                                       @
                                                                       
                                                                       
                                                                       \subsubsection*{Priors informed by expert knowledge}
                                                                       
                                                                       When no data were available, expert knowledge was used to estimate the central tendency and confidence interval for a trait parameter.
                                                                       Again, optimization was used to fit a probability distribution to these constraints.
                                                                       
                                                                       
                                                                       The minimum temperature of photosynthesis for C4 grasses was given a prior based on expert knowledge with a mean of $10^o$C and a $95\%\text{CI}=[8,12]^o$C that fits a normal ($\mu = 10$, $\sigma=1.02$) distribution (Don Ort, UIUC, personal communication, 2010).
                                                                       <<echo=FALSE, results = hide>>=
                                                                       prior.dists <- rbind(prior.dists,
                                                                       'Vm_low_temp' = data.frame(distribution = 'norm', 
                                                                       a = 10, 
                                                                       b = tabnum(2/qnorm(0.975)), 
                                                                       ## qnorm(0.975) = 1.96 
                                                                       ## the distance in terms of sigma of the 95%CI from the mean 
                                                                       n = 0)) 
                                                                       pmt.dens <- prior.density('norm', 10, 1.02)
                                                                       prior.figures[['Vm_low_temp']]  <- priorfig(priordensity = pmt.dens, 
                                                                       trait = 'Vm_low_temp') +
                                                                       geom_vline(aes(xintercept = c(8, 10, 12), linetype = c(2,1,2)), color = 'grey')
                                                                       
                                                                       
                                                                       @ 
                                                                       
                                                                       
                                                                       The growth respiration factor is the proportion of daily carbon gain lost to growth respiration.
                                                                       Because it is a proportion, the beta distribution was fit with a mean set equal to the ED2 default parameter value, $0.33$ and a $95\% \text{CI} = [0.05,0.60]$, conservatively based on the range of construction costs reviewed by \citet{amthor2000mdp}.
                                                                       % cost function:
                                                                       % $$\text{min}( |F^{-1}(0.05|a,b) - 0.15| + |F^{-1}(0.95|a,b) - 0.60| + |\text{mode}(\text{Beta}(a,b)) - 0.33| )$$
                                                                       <<echo=FALSE,results=hide,label=growthresp>>=
                                                                       
                                                                       parms <- DEoptim(fn = prior.fn, 
                                                                       lower = c(0, 0), 
                                                                       upper = c(10000, 10000), 
                                                                       x=c(0.05, 0.6, 0.33),
                                                                       alpha = 0.05,
                                                                       distn = 'beta',
                                                                       central.tendency = 'mean',
                                                                       trait = 'growthresp')$optim$bestmem
                                                                       
                                                                       prior.dists <- rbind(prior.dists,
                                                                       'growth_resp_factor' = data.frame(distribution = 'beta', 
                                                                       a = tabnum(parms[1]),
                                                                       b = tabnum(parms[2]),
                                                                       n=0))
                                                                       
                                                                       gr.dens <- prior.density('beta', prior.dists['growth_resp_factor', 'a'], prior.dists['growth_resp_factor', 'b'])
                                                                       prior.figures[['growth_resp_factor']]  <- priorfig(priordensity = gr.dens, 
                                                                       trait = 'growth_resp_factor') +
                                                                       geom_vline(aes(xintercept = c(0.15, 0.33, 0.6), linetype = c(2,1,2)), color = 'grey')
                                                                       
                                                                       
                                                                       @
                                                                       
                                                                       Seedling mortality factor represents the proportion of carbon allocated to reproduction that goes directly to the litter pool.
                                                                       Given the default ED2 parameter is 0.95, we stated a beta prior with a median at $0.95$, and a $95\% \text{CI} = [^2/_3, 1]$.
                                                                       %  $$\text{min}( |F^{-1}(0.025|a,b) - 0.5| + |F^{-1}(0.975|a,b) - 1 | + |\text{mode}(\text{Beta}(a,b)) - 0.95| )$$
                                                                       <<echo=FALSE,results=hide>>=
                                                                       parms <- DEoptim(fn = prior.fn, 
                                                                       lower = c(0, 0), 
                                                                       upper = c(1000, 1000), 
                                                                       x=c(1/2, 1, 0.95), 
                                                                       alpha = 0.05, 
                                                                       distn = 'beta', 
                                                                       central.tendency = 'median', trait = 'seedling_mortality')$optim$bestmem
                                                                       
                                                                       prior.dists <- rbind(prior.dists,
                                                                       'seedling_mortality' = data.frame(distribution = 'beta', 
                                                                       a = tabnum(parms[1]),
                                                                       b = tabnum(parms[2]),
                                                                       n=0))
                                                                       
                                                                       
                                                                       ##### Prior Plot
                                                                       sm.dens <- with(prior.dists['seedling_mortality',], prior.density('beta', a, b))
                                                                       
                                                                       prior.figures[['seedling_mortality']]  <- priorfig(priordensity = sm.dens, 
                                                                       trait = 'seedling_mortality', 
                                                                       xlim = c(0.4, 1)) +
                                                                       geom_vline(aes(xintercept = c(0.5, 0.95, 1), linetype = c(2,1,2)), color = 'grey')
                                                                       
                                                                       
                                                                       @ 
                                                                       
                                                                       The mortality factor in ED2 is the rate parameter in the negative exponential relationship between carbon balance and mortality \citep{medvigy2009mse}.
                                                                       The default parameter for all plant functional types (PFT's) in ED2 is $20$, and our weakly informed gamma prior sets this as the median and gives a $95\% \text{CI}=[5,80]$. 
%% $$\text{min}( |F^{-1}(0.05|a,b) - 5| + |\text{mode}(\text{Gamma}(a,b)) - 20| + |F^{-1}(0.95|a,b) - 100| )$$
  <<echo=FALSE,results=hide,label=mort2>>=
  
  parms <- DEoptim(fn = prior.fn, 
                   lower = c(0, 0), 
                   upper = c(1000, 1000), 
                   x=c(5, 80, 20),
                   distn = 'gamma',
                   alpha = 0.05,
                   central.tendency = 'median')$optim$bestmem

prior.dists <- rbind(prior.dists, 'mort2' = data.frame(distribution = 'gamma', 
                                                       a = tabnum(parms[1]), 
                                                       b = tabnum(parms[2]), 
                                                       n = 0))

##### Prior Plot
mort2.density <- with(prior.dists['mort2',], prior.density('gamma', a, b))

prior.figures[['mort2']] <- priorfig(priordensity = mort2.density, 
                                     trait = 'mort2') +
  geom_vline(aes(xintercept = c(5, 20, 80), linetype = c(2,1,2)), color = 'grey')


@ 


Reproductive allocation represents the proportion of carbon in the storage pool allocated to reproduction. 
This parameter is a proportion and has a default value of $0.33$ in ED.  
The $\operatorname{Beta}(2,4)$ distribution has a mean of $^1/_3$ and a $95\%\text{CI}=[0.05, 0.72]$ representing relatively high uncertainty.
This distribution implies that the plant may allocate any fraction of the carbon pool to reproduction between but not equal to $0$ and $1$ and has an $80\%$ probability that less than half of the carbon pool will be allocated to reproduction.
<<echo=FALSE,results=hide>>=
  prior.dists <- rbind(prior.dists,
                       'r_fract' = data.frame(distribution = 'beta', 
                                              a = 2,
                                              b = 4,
                                              n=0))

rfract.dens <- with(prior.dists['r_fract',], prior.density('beta', a, b))
prior.figures[['r_fract']]  <- priorfig(priordensity = rfract.dens, trait = 'r_fract') +
  geom_vline(aes(xintercept = 1/3, linetype = 1), color = 'grey')

@ 


%% TODO *** data ticks on the X are too small, data intervals for leaf width, Vcmax, and root resp can be spaced out more vertically  ***
  
  <<fig=FALSE,include=FALSE,results=hide,echo=FALSE>>=
  
  pdf('priorfiguresi.pdf', height = 3, width = 6)
for(x in rownames(prior.dists)){
  print(x)
  print(prior.figures[[x]])
}
dev.off()  
system("pdflatex priorfigures.tex", ignore.stdout = TRUE)
#do.call(grid.arrange, c(prior.figures, nrow = 4, ncol = 4))

@ 

% \section{Summary of Prior distributions}


<<echo=FALSE,label=priors,results=hide>>=
  
  ### commented-out code below extracts compares priors computed her to priors from database
  ###     alternative would be to write priors directly to database
  ###     db connection requires password
  ## con <- function() {query.bety.con(username = "ebi_analys_user", password = "****", 
  ##                                   host = "ebi-forecast", dbname = "ebi_analysis")}
  ## priors <- query.bety("select priors.id, name, phylogeny, distn, parama, paramb, n, units, author from priors join variables on priors.variable_id = variables.id join citations on priors.citation_id = citations.id where priors.id in (select prior_id from pfts_priors where pft_id = (select id from pfts where name = 'ebifarm.pavi'));", con())
  ## ## Test: do priors in database match priors computed here?
  ## rownames(priors) <- priors$name
  ## match.names <- rownames(prior.dists) %in% priors$name
  ## if(all(match.names)) { 
##   for(name in rownames(prior.dists)) {
##     if(!prior.dists[name,c('a','b')] == priors[name, c('parama', 'paramb')]){
##       print(name)
##       print(prior.dists[name,c('a','b')])
##       print(priors[name, c('parama', 'paramb')])
##  }}}
## write.csv(format(priors, scientific = FALSE), 'priors.csv')

## load units, author, phylogeny etc. from csv (rather than connecting to database)
priors <- read.csv('priors.csv')
priors <- priors[, !colnames(priors) %in% c('distn','parama', 'paramb', 'n', 'X', 'id')]
prior.dists$name <- rownames(prior.dists)

priors <- merge(prior.dists, priors, sort = FALSE)
priors$name <- trait.dictionary(priors$name)$figid
levels(priors$distribution)[levels(priors$distribution) == 'lognormal'] <- 'lnorm'

### Calculate summary statistics from prior distributions and add to priors
traits <- rownames(priors)
priorstats <- t(sapply(seq(traits), 
                       function(x) prettyNum(pdf.stats(priors$distribution[x], 
                                                       priors$a[x], 
                                                       priors$b[x])[-2], 
                                             scientific = FALSE, digits=2)))
priors <- data.frame(priors, priorstats, row.names = priors$name)

## Convert units to latex format
priors$units <- gsub("_2","$_2$", priors$units)
priors$units <- gsub("m2","m$^2$", priors$units)
priors$units <- gsub("-2","$^{-2}$", priors$units)
priors$units <- gsub("-1","$^{-1}$", priors$units)
priors$units <- gsub("H2O","H$_2$O", priors$units)
priors$units <- gsub("%","\\%", priors$units)

## Capitalize 'graminoid', 'monocots'
priors$units <- gsub("graminoid","Graminoid", priors$units)
priors$units <- gsub("monocots","Monocots", priors$units)
priors$units <- gsub("C4 grasses","C4 Grass", priors$units)

## Convert author name to bibtex citations
priors$author <- tolower(priors$author)
priors$author[which(priors$author=='lebauer')]<-'*'
priors$author[which(priors$author=='feng')]<-'*'
priors$author[which(priors$author=='wullschleger')]<-'wullschleger1993blc'
priors$author[which(priors$author=='wright')]<-'wright2004wwl'
priors$author[which(priors$author=='skillman')]<-'skillman2008qyv'
priors$author[which(priors$author=='shipley')]<-'shipley2006nar'
priors$author[which(priors$author=='oyarzabal')]<-'oyarzabal2008tdb'
priors$author[which(priors$author=='liu')]<-'liu2008sid'
priors$author[which(priors$author=='gill')]<-'gill2000gpr'
priors$author[which(priors$author=='jongejans')]<-'jongejans1999msd'
priors$author[which(priors$name  =='stomatal_slope')]<-'collatz1992cps,norman1989cp'
priors$author[which(priors$name  =='Vm_low_temp')]<-'Don Ort, personal communication'
priors$author[which(priors$name  =='stomatal_slope')]<-'Andrew Leakey, personal communication'

priors$author[which(priors$name=='Fine Root Allocation')]<-'chapin2002pte'
priors$author[which(priors$author == 'tjoelker')] <- 'tjoelker2005llr'
bibtex<-which(priors$author != '*' & (!priors$name %in% c('Vm_low_temp', "stomatal_slope")))
priors$author[bibtex] <- paste("\\citep{", priors$author[bibtex], "}", sep = "")

## Convert distribution names to human readable form
priors$distribution <- sapply(priors$distribution, function(x) switch(x,'gamma' = 'Gamma',
                                                                      'weibull' = 'Weibull',
                                                                      'f' = 'F',
                                                                      'beta' = 'Beta', 
                                                                      'lnorm' = 'log-Normal', 
                                                                      'norm' = 'Normal'))
## Convert column names to human readable form
priors <- with(priors, data.frame(Parameter = name,
                                  Units = units,
                                  Clade = phylogeny, 
                                  Distribution= distribution,
                                  a = a, 
                                  b = b, 
                                  N = as.integer(n), 
                                  mean = mean,
                                  LCL = lcl,
                                  UCL = ucl,
                                  Citation = author))

## save table as csv
write.csv(priors, file = 'prior_table.csv')
@ 


<<echo=FALSE,results=hide>>=
  
  print(xtable(x = priors, 
               label = "tab:priors", 
               caption="{\\bf Prior Distributions} Prior distributions used in meta-analysis and model parameterization. Prior sources come from citations as indicated except $^\\ast$ by authors or $^\\dagger$ based on default ED2 parameterizations, as described in text. The 'Clade' column indicates the group of plants for which the priors were derived.",
               caption.placement = NULL,
               align=NULL), 
        file = "priortab.tex",
        floating.environment= "sidewaystable",
        #      table.placement = "ht",
        include.rownames = FALSE, 
        sanitize.text.function = function(x) gsub("%", "\\\\%", x),
        sanitize.colnames.function = identity)

@ 

