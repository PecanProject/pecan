#write met to xml format
#example: <path2>/home/araiho/linkages_ens_hf_met//bcc-csm1-1_001.01/climate.Rdata</path2> 

#ens_wts <- read.csv(settings$run$inputs$ensembleweights$path)
ens_wts <- read.csv('/data/dbfiles/met_data/HARVARD/weights/monte-carlo-ensemble_weights-HARVARD-prism.csv')

clim_mods <- unique(ens_wts$climate_model)

avg_wt <- numeric(length(clim_mods))

for(i in 1:length(clim_mods)){
  avg_wt[i] <- mean(ens_wts[ens_wts$climate_model==clim_mods[i],'weights'])
}

clim_use <- sample(x = clim_mods,size = 300,prob = avg_wt,replace = T)
rbind(sort(avg_wt),sort(table(clim_use)))


metdir <- '/home/araiho/HF_MET/'

if(FALSE){
  files_out <- list.files(metdir)
  name <- numeric(length(files_out))
}

clim_use <- clim_mods

name <- numeric(length(clim_use))

for(i in 1:length(clim_use)){
  name[i] <- paste0('<path',i+1,'>',metdir,clim_use[i],'/climate.Rdata</path',i+1,'>')
}

writeLines(name)

#copy and paste from console to inputs tag in xml