#query_pfts
setwd("/projectnb/dietzelab/dongchen/All_NEON_SDA")
settings <- PEcAn.settings::read.settings("test_NEON2.xml")


#prepare obs
load("Obs_2012_2021/obs_cov.Rdata")
load("Obs_2012_2021/obs_mean.Rdata")


#prepare samples
get.parameter.samples(settings, ens.sample.method = settings$ensemble$samplingspace$parameters$method)

#prepare settings
new.settings <- PEcAn.settings::prepare.settings(settings)


#debug on sda.enkf_Multisite.R
settings <- new.settings

#run sda main function
sda.enkf.multisite(settings, 
                   obs.mean, 
                   obs.cov, 
                   Q = NULL, 
                   restart = FALSE, 
                   forceRun = TRUE, 
                   keepNC = TRUE, 
                   control=list(trace = TRUE,
                                FF = FALSE,
                                interactivePlot = FALSE,
                                TimeseriesPlot = FALSE,
                                BiasPlot = FALSE,
                                plot.title = NULL,
                                facet.plots = FALSE,
                                debug = FALSE,
                                pause = FALSE,
                                Profiling = FALSE,
                                OutlierDetection=FALSE))