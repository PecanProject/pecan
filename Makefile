NCPUS ?= 1

BASE := logger utils db settings visualization qaqc

MODELS := biocro clm45 dalec ed fates gday jules linkages \
				lpjguess maat maespa preles sipnet

MODULES := allometry assim.batch assim.sequential benchmark \
				 data.atmosphere data.hydrology data.land \
				 data.mining data.remote emulator meta.analysis \
				 photosynthesis priors rtm uncertainty

BASE := $(BASE:%=base/%)
MODELS := $(MODELS:%=models/%)
MODULES := $(MODULES:%=modules/%)
ALL_PKGS := $(BASE) $(MODULES) $(MODELS)

BASE_I := $(BASE:%=.install/%)
MODELS_I := $(MODELS:%=.install/%)
MODULES_I := $(MODULES:%=.install/%)
ALL_PKGS_I := $(BASE_I) $(MODULES_I) $(MODELS_I)

BASE_C := $(BASE:%=.check/%)
MODELS_C := $(MODELS:%=.check/%)
MODULES_C := $(MODULES:%=.check/%)
ALL_PKGS_C := $(BASE_C) $(MODULES_C) $(MODELS_C)

BASE_T := $(BASE:%=.test/%)
MODELS_T := $(MODELS:%=.test/%)
MODULES_T := $(MODULES:%=.test/%)
ALL_PKGS_T := $(BASE_T) $(MODULES_T) $(MODELS_T)

BASE_D := $(BASE:%=.doc/%)
MODELS_D := $(MODELS:%=.doc/%)
MODULES_D := $(MODULES:%=.doc/%)
ALL_PKGS_D := $(BASE_D) $(MODULES_D) $(MODELS_D)

.PHONY: all install check test document

all: install document

document: $(ALL_PKGS_D) .doc/base/all
install: $(ALL_PKGS_I) .install/base/all
check: $(ALL_PKGS_C) .check/base/all
test: $(ALL_PKGS_T) .test/base/all

### Dependencies
.doc/base/all: $(ALL_PKGS_D)
.install/base/all: $(ALL_PKGS_I)
.check/base/all: $(ALL_PKGS_C)
.test/base/all: $(ALL_PKGS_T)

depends = .doc/$(1) .install/$(1) .check/$(1) .test/$(1)

$(call depends,base/db): .install/base/logger .install/base/utils
$(call depends,base/settings): .install/base/logger .install/base/utils .install/base/db
$(call depends,base/visualization): .install/base/logger .install/base/db
$(call depends,base/qaqc): .install/base/logger
$(call depends,modules/data.atmosphere): .install/base/logger .install/base/utils
$(call depends,modules/data.land): .install/base/logger .install/base/db .install/base/utils
$(call depends,modules/meta.analysis): .install/base/logger .install/base/utils .install/base/db
$(call depends,modules/priors): .install/base/logger .install/base/utils
$(call depends,modules/assim.batch): .install/base/logger .install/base/utils .install/base/db .install/modules/meta.analysis
$(call depends,modules/rtm): .install/base/logger .install/modules/assim.batch
$(call depends,modules/uncertainty): .install/base/logger .install/base/utils .install/modules/priors
$(call depends,models/template): .install/base/logger .install/base/utils
$(call depends,models/biocro): .install/base/logger .install/base/utils .install/base/settings .install/base/db .install/modules/data.atmosphere .install/modules/data.land

clean:
	rm -rf .install .check .test .doc
	find modules/rtm/src \( -name \*.mod -o -name \*.o -o -name \*.so \) -delete

.install/devtools:
	Rscript -e "if(!require('devtools')) install.packages('devtools', repos = 'http://cran.rstudio.com', Ncpus = ${NCPUS})"
	mkdir -p $(@D)
	echo `date` > $@

.install/roxygen2:
	Rscript -e "if(!require('roxygen2')) install.packages('roxygen2', repos = 'http://cran.rstudio.com', Ncpus = ${NCPUS})"
	mkdir -p $(@D)
	echo `date` > $@

.install/testthat:
	Rscript -e "if(!require('testthat')) install.packages('testthat', repos = 'http://cran.rstudio.com', Ncpus = ${NCPUS})"
	mkdir -p $(@D)
	echo `date` > $@

depends_R_pkg = Rscript -e "devtools::install_deps('$(strip $(1))', threads = ${NCPUS});"
install_R_pkg = Rscript -e "devtools::install('$(strip $(1))', Ncpus = ${NCPUS});"
check_R_pkg = Rscript scripts/check_with_errors.R $(strip $(1))
test_R_pkg = Rscript -e "devtools::test('"$(strip $(1))"', reporter = 'stop')"
doc_R_pkg = Rscript -e "devtools::document('"$(strip $(1))"')"

$(ALL_PKGS_I) $(ALL_PKGS_C) $(ALL_PKGS_T) $(ALL_PKGS_D): .install/devtools .install/roxygen2 .install/testthat

.SECONDEXPANSION:
.doc/%: $$(wildcard %/**/*) $$(wildcard %/*)
	$(call depends_R_pkg, $(subst .doc/,,$@))
	$(call doc_R_pkg, $(subst .doc/,,$@))
	mkdir -p $(@D)
	echo `date` > $@

.install/%: $$(wildcard %/**/*) $$(wildcard %/*) .doc/%
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

