BASE_PACKAGES := utils db settings visualization

MODELS := biocro clm45 dalec ed gday jules linkages \
	      lpjguess maat maespa preles sipnet


MODULES := allometry assim.batch assim.sequential benchmark \
	       data.atmosphere data.hydrology data.land \
	       data.mining data.remote emulator meta.analysis \
	       photosynthesis priors rtm uncertainty

.PHONY: all ${BASE_PACKAGES} ${MODELS} ${MODULES}

all: .install PEcAn.all ${BASE_PACKAGES} ${MODELS} ${MODULES}

clean:
	rm -rf .install

.install:
	mkdir -p $@

#### DEPENDENCY MAPPING ####

PEcAn.all: ${BASE_PACKAGES} ${MODULES} ${MODELS}

${MODELS}: ${BASE_PACKAGES} ${MODULES}

${BASE_PACKAGES} ${MODELS} ${MODULES}: .install/devtools

.install/devtools:
	Rscript -e "req <- require('devtools'); if(!req) install.packages('devtools', repos = 'http://cran.rstudio.com')"
	echo `date` > .install/devtools

#### BASE PACKAGES ####

PEcAn.all: .install/all

.install/all: $(wildcard all/**/*)
	Rscript -e "devtools::install('all')"
	echo `date` > .install/all

utils: .install/utils

.install/utils: $(wildcard utils/**/*)
	Rscript -e "devtools::install('utils')"
	echo `date` > .install/utils

db: utils .install/db

.install/db: $(wildcard db/**/*)
	Rscript -e "devtools::install('db')"
	echo `date` > .install/db

settings: utils db .install/settings

.install/settings: $(wildcard settings/**/*)
	Rscript -e "devtools::install('settings')"
	echo `date` > .install/settings

visualization: db .install/visualization

.install/visualization: $(wildcard visualization/**/*)
	Rscript -e "devtools::install('visualization')"
	echo `date` > .install/visualization

#### MODULES ####

allometry: .install/allometry

.install/allometry: $(wildcard modules/allometry/**/*)
	Rscript -e "devtools::install('modules/allometry')"
	echo `date` > .install/allometry

assim.batch: .install/assim.batch

.install/assim.batch: $(wildcard modules/assim.batch/**/*)
	Rscript -e "devtools::install('modules/assim.batch')"
	echo `date` > .install/assim.batch

assim.sequential: .install/assim.sequential

.install/assim.sequential: $(wildcard modules/assim.sequential/**/*)
	Rscript -e "devtools::install('modules/assim.sequential')"
	echo `date` > .install/assim.sequential

benchmark: .install/benchmark

.install/benchmark: $(wildcard modules/benchmark/**/*)
	Rscript -e "devtools::install('modules/benchmark')"
	echo `date` > .install/benchmark

data.atmosphere: utils .install/data.atmosphere

.install/data.atmosphere: $(wildcard modules/data.atmosphere/**/*)
	Rscript -e "test <- require('REddyProc'); if (!test) devtools::install_github('rforge/reddyproc', subdir = 'pkg/REddyProc')"
	Rscript -e "devtools::install('modules/data.atmosphere')"
	echo `date` > .install/data.atmosphere

data.hydrology: .install/data.hydrology

.install/data.hydrology: $(wildcard modules/data.hydrology/**/*)
	Rscript -e "devtools::install('modules/data.hydrology')"
	echo `date` > .install/data.hydrology

data.land: db utils .install/data.land

.install/data.land: $(wildcard modules/data.land/**/*)
	Rscript -e "devtools::install('modules/data.land')"
	echo `date` > .install/data.land

data.mining: .install/data.mining

.install/data.mining: $(wildcard modules/data.mining/**/*)
	Rscript -e "devtools::install('modules/data.mining')"
	echo `date` > .install/data.mining

data.remote: .install/data.remote

.install/data.remote: $(wildcard modules/data.remote/**/*)
	Rscript -e "devtools::install('modules/data.remote')"
	echo `date` > .install/data.remote

emulator: .install/emulator

.install/emulator: $(wildcard modules/emulator/**/*)
	Rscript -e "devtools::install('modules/emulator')"
	echo `date` > .install/emulator

meta.analysis: utils db .install/meta.analysis

.install/meta.analysis: $(wildcard modules/meta.analysis/**/*)
	Rscript -e "devtools::install('modules/meta.analysis')"
	echo `date` > .install/meta.analysis

photosynthesis: .install/photosynthesis

.install/photosynthesis: $(wildcard modules/photosynthesis/**/*)
	Rscript -e "devtools::install('modules/photosynthesis')"
	echo `date` > .install/photosynthesis

priors: utils .install/priors

.install/priors: $(wildcard modules/priors/**/*)
	Rscript -e "devtools::install('modules/priors')"
	echo `date` > .install/priors

rtm: .install/rtm

.install/rtm: $(wildcard modules/rtm/**/*)
	Rscript -e "devtools::install('modules/rtm')"
	echo `date` > .install/rtm

uncertainty: utils priors .install/uncertainty

.install/uncertainty: $(wildcard modules/uncertainty/**/*)
	Rscript -e "devtools::install('modules/uncertainty')"
	echo `date` > .install/uncertainty

#### MODELS ####

biocro: .install/biocro

.install/biocro: $(wildcard models/biocro/**/*)
	Rscript -e "devtools::install('models/biocro')"
	echo `date` > .install/biocro

clm45: .install/biocro

.install/clm45: $(wildcard models/biocro/**/*)
	Rscript -e "devtools::install('models/clm45')"
	echo `date` > .install/clm45

dalec: .install/dalec

.install/dalec: $(wildcard models/dalec/**/*)
	Rscript -e "devtools::install('models/dalec')"
	echo `date` > .install/dalec

ed: .install/ed

.install/ed: $(wildcard models/ed/**/*)
	Rscript -e "devtools::install('models/ed')"
	echo `date` > .install/ed

gday: .install/gday

.install/gday: $(wildcard models/gday/**/*)
	Rscript -e "devtools::install('models/gday')"
	echo `date` > .install/gday

jules: .install/jules

.install/jules: $(wildcard models/jules/**/*)
	Rscript -e "devtools::install('models/jules')"
	echo `date` > .install/jules

linkages: .install/linkages

.install/linkages: $(wildcard models/linkages/**/*)
	Rscript -e "devtools::install('models/linkages')"
	echo `date` > .install/linkages

lpjguess: .install/lpjguess

.install/lpjguess: $(wildcard models/lpjguess/**/*)
	Rscript -e "devtools::install('models/lpjguess')"
	echo `date` > .install/lpjguess

maat: .install/biocro

.install/maat: $(wildcard models/biocro/**/*)
	Rscript -e "devtools::install('models/maat')"
	echo `date` > .install/maat

maespa: .install/biocro

.install/maespa: $(wildcard models/biocro/**/*)
	Rscript -e "devtools::install('models/maespa')"
	echo `date` > .install/maespa

preles: .install/preles

.install/preles: $(wildcard models/preles/**/*)
	Rscript -e "devtools::install('models/preles')"
	echo `date` > .install/preles

sipnet: .install/sipnet

.install/sipnet: $(wildcard models/sipnet/**/*)
	Rscript -e "devtools::install('models/sipnet')"
	echo `date` > .install/sipnet

