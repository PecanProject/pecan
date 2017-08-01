#devtools::load_all('assim.batch')
#devtools::load_all('rtm')
library(PEcAnRTM)

#observed <- prospect(defparam('prospect_5'), 5)[,1] + generate.noise()
data(testspec)
observed <- testspec_ACRU[,5]
model <- function(x) prospect(x, 5)[,1]
prior <- PEcAn.assim.batch::prospect_bt_prior(5)
custom_settings <- list()
samples <- invert_bt(observed = observed, model = model, prior = prior,
                     custom_settings = list())

s <- getSample(samples, start = 400, coda = TRUE)
traceplot(s[,2])
