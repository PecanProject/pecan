BASE := utils db settings visualization

MODELS := biocro clm45 dalec ed fates gday jules linkages \
				lpjguess maat maespa preles sipnet

MODULES := allometry assim.batch assim.sequential benchmark \
				 data.atmosphere data.hydrology data.land \
				 data.mining data.remote emulator meta.analysis \
				 photosynthesis priors rtm uncertainty

MODELS := $(MODELS:%=models/%)
MODULES := $(MODULES:%=modules/%)
ALL_PKGS := $(BASE) $(MODELS) $(MODULES) models/template

BASE_I := $(BASE:%=.install/%)
MODELS_I := $(MODELS:%=.install/%)
MODULES_I := $(MODULES:%=.install/%)
ALL_PKGS_I := $(BASE_I) $(MODELS_I) $(MODULES_I) .install/models/template

BASE_C := $(BASE:%=.check/%)
MODELS_C := $(MODELS:%=.check/%)
MODULES_C := $(MODULES:%=.check/%)
ALL_PKGS_C := $(BASE_C) $(MODELS_C) $(MODULES_C) .check/models/template

BASE_T := $(BASE:%=.test/%)
MODELS_T := $(MODELS:%=.test/%)
MODULES_T := $(MODULES:%=.test/%)
ALL_PKGS_T := $(BASE_T) $(MODELS_T) $(MODULES_T) .test/models/template

BASE_D := $(BASE:%=.doc/%)
MODELS_D := $(MODELS:%=.doc/%)
MODULES_D := $(MODULES:%=.doc/%)
ALL_PKGS_D := $(BASE_D) $(MODELS_D) $(MODULES_D) .doc/models/template

.PHONY: all install check test document

all: install

document: .doc/all
install: .install/all 
check: .check/all
test: .test/all 

### Dependencies
.doc/all: $(ALL_PKGS_D)
.install/all: $(ALL_PKGS_I)
.check/all: $(ALL_PKGS_C)
.test/all: $(ALL_PKGS_T)

depends = .install/$(1) .doc/$(1) .check/$(1) .test/$(1)

$(call depends,db): .install/utils
$(call depends,settings): .install/utils .install/db
$(call depends,visualization): .install/db .install/shiny
$(call depends,modules/data.atmosphere): .install/utils .install/reddyproc
$(call depends,modules/data.land): .install/db .install/utils
$(call depends,modules/meta.analysis): .install/utils .install/db
$(call depends,modules/priors): .install/utils
$(call depends,modules/assim.batch): .install/utils .install/db .install/modules/meta.analysis 
$(call depends,modules/rtm): .install/modules/assim.batch
$(call depends,models/template): .install/utils
$(call depends,models/biocro): .install/utils .install/modules/data.atmosphere .install/modules/data.land

$(MODELS_I): .install/models/template


clean:
	rm -rf .install .check .test .doc

.install/devtools:
	Rscript -e "if(!require('devtools')) install.packages('devtools', repos = 'http://cran.rstudio.com')"
	mkdir -p $(@D)
	echo `date` > $@

.install/shiny:
	Rscript -e "if(!require('shiny')) install.packages('shiny', repos = 'http://cran.rstudio.com')"
	mkdir -p $(@D)
	echo `date` > $@

.install/reddyproc:
	Rscript -e "test <- require('REddyProc'); if (!test) devtools::install_github('rforge/reddyproc', subdir = 'pkg/REddyProc')"
	mkdir -p $(@D)
	echo `date` > $@

install_R_pkg = Rscript -e "devtools::install('$(strip $(1))');"
check_R_pkg = Rscript -e "devtools::check('"$(strip $(1))"')"
test_R_pkg = Rscript -e "devtools::test('"$(strip $(1))"')"
doc_R_pkg = Rscript -e "devtools::document('"$(strip $(1))"')"

$(ALL_PKGS_I) $(ALL_PKGS_C) $(ALL_PKGS_T) $(ALL_PKGS_D): .install/devtools

.SECONDEXPANSION:
.doc/%: $$(wildcard %/**/*) $$(wildcard %/*)
	$(call doc_R_pkg, $(subst .doc/,,$@))
	mkdir -p $(@D)
	echo `date` > $@

.install/%: $$(wildcard %/**/*) $$(wildcard %/*)
	$(call install_R_pkg, $(subst .install/,,$@))
	mkdir -p $(@D)
	echo `date` > $@

.check/%: $$(wildcard %/**/*) $$(wildcard %/*)
	$(call check_R_pkg, $(subst .check/,,$@))
	mkdir -p $(@D)
	echo `date` > $@

.test/%: $$(wildcard %/**/*) $$(wildcard %/*)
	$(call test_R_pkg, $(subst .test/,,$@))
	mkdir -p $(@D)
	echo `date` > $@

