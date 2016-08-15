rm(list=ls())
library(devtools)
load_all('/fs/data2/rykelly/pecan/settings')

template.settings.file = '/fs/data2/rykelly/mandifore/testruns/015_mandSE_pecan.template.01.xml'
sitegroup.id <- 1000000002
start.date = "2004/01/01"
end.date = "2004/01/31"
n.site <- 20

out.dir = '/fs/data2/rykelly/mandifore/testruns/017_mandSE_test.02'

template.settings <- read.settings(template.settings.file)
  template.settings$run$start.date <- start.date
  template.settings$run$end.date <- end.date


all.site.ids <- db.query(paste("SELECT id FROM sitegroups_sites WHERE sitegroup_id =", sitegroup.id), 
  params=template.settings$database$bety)$id

site.ids <- sample(all.site.ids, n.site, replace=F)

settings <- createMultiSiteSettings(template.settings, site.ids, out.dir)

dir.create(out.dir, showWarnings=F)
saveXML(listToXml(settings, "pecan.multi"), file=file.path(out.dir, "pecan.xml"))
