NCPUS ?= 1

BASE := logger utils db settings visualization qaqc remote workflow

MODELS := biocro clm45 dalec ed fates gday jules linkages \
				lpjguess maat maespa preles sipnet dvmdostem template

MODULES := allometry assim.batch assim.sequential benchmark \
				 data.atmosphere data.hydrology data.land \
				 data.mining data.remote emulator meta.analysis \
				 photosynthesis priors rtm uncertainty

SHINY := BenchmarkReport BrownDog Data-Ingest Elicitation Pecan.depend \
				ViewMet global-sensitivity workflowPlot

BASE := $(BASE:%=base/%)
MODELS := $(MODELS:%=models/%)
MODULES := $(MODULES:%=modules/%)
ALL_PKGS := $(BASE) $(MODULES) $(MODELS)
SHINY := $(SHINY:%=shiny/%)

BASE_I := $(BASE:%=.install/%)
MODELS_I := $(MODELS:%=.install/%)
MODULES_I := $(MODULES:%=.install/%)
ALL_PKGS_I := $(BASE_I) $(MODULES_I) $(MODELS_I)
SHINY_I := $(SHINY:shiny/%=.shiny_depends/%)

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

.PHONY: all install check test document shiny

all: install document

document: $(ALL_PKGS_D) .doc/base/all
install: $(ALL_PKGS_I) .install/base/all
check: $(ALL_PKGS_C) .check/base/all
test: $(ALL_PKGS_T) .test/base/all
shiny: $(SHINY_I)

depends = .doc/$(1) .install/$(1) .check/$(1) .test/$(1)

# Make the timestamp directories if they don't exist yet
.doc .install .check .test .shiny_depends $(call depends,base) $(call depends,models) $(call depends,modules):
	mkdir -p $@

### Dependencies

# models import Roxygen docs from *installed* version of template,
# so changes in template mean the models need to be redocumented
$(subst .doc/models/template,,$(MODELS_D)): .install/models/template

### Order-only dependencies
# (i.e. prerequisites must exist before building target, but
# target need not be rebuilt when a prerequisite changes)

.doc/base/all: | $(ALL_PKGS_D)
.install/base/all: | $(ALL_PKGS_I)
.check/base/all: | $(ALL_PKGS_C)
.test/base/all: | $(ALL_PKGS_T)

$(subst .install/base/logger,,$(ALL_PKGS_I)): | .install/base/logger
$(subst .doc/base/logger,,$(ALL_PKGS_D)): | .install/base/logger

$(call depends,base/utils): | .install/base/remote
$(call depends,base/db): | .install/base/utils
$(call depends,base/qaqc): | .install/base/utils
$(call depends,base/settings): | .install/base/utils .install/base/db
$(call depends,base/visualization): | .install/base/db
$(call depends,base/workflow): | .install/base/db .install/base/settings
$(call depends,modules/allometry): | .install/base/db
$(call depends,modules/assim.batch): | .install/base/utils .install/base/db .install/base/settings .install/base/remote .install/modules/meta.analysis
$(call depends,modules/assim.sequential): | .install/base/remote
$(call depends,modules/benchmark): | .install/base/db .install/base/remote .install/base/settings .install/base/utils .install/modules/data.land
$(call depends,modules/data.atmosphere): | .install/base/utils .install/base/remote .install/base/db
$(call depends,modules/data.hydrology): | .install/base/utils
$(call depends,modules/data.land): | .install/base/db .install/base/utils .install/base/settings .install/base/remote
$(call depends,modules/data.mining): | .install/base/utils
$(call depends,modules/data.remote): | .install/base/remote
$(call depends,modules/meta.analysis): | .install/base/utils .install/base/db .install/base/settings .install/modules/priors
$(call depends,modules/priors): | .install/base/utils
$(call depends,modules/rtm): | .install/modules/assim.batch .install/base/utils .install/models/ed
$(call depends,modules/uncertainty): | .install/base/utils .install/base/db .install/modules/priors .install/modules/emulator
$(call depends,models/biocro): | .install/mockery .install/base/utils .install/base/settings .install/base/db .install/modules/data.atmosphere .install/modules/data.land .install/base/remote
$(call depends,models/cable): | .install/base/utils
$(call depends,models/clm45): | .install/base/utils
$(call depends,models/dalec): | .install/base/remote
$(call depends,models/dvmdostem): | .install/base/utils
$(call depends,models/ed): | .install/base/utils .install/base/remote .install/base/settings
$(call depends,models/fates): | .install/base/utils .install/base/remote
$(call depends,models/gday): | .install/base/utils .install/base/remote
$(call depends,models/jules): | .install/base/utils .install/base/remote
$(call depends,models/linkages): | .install/base/utils .install/base/remote
$(call depends,models/lpjguess): | .install/base/utils .install/base/remote
$(call depends,models/maat): | .install/base/utils .install/base/remote .install/base/settings
$(call depends,models/maespa): | .install/base/utils .install/base/remote .install/modules/data.atmosphere
$(call depends,models/preles): | .install/base/utils .install/modules/data.atmosphere
$(call depends,models/sipnet): | .install/base/utils .install/base/remote .install/modules/data.atmosphere
$(call depends,models/template): | .install/base/utils

clean:
	rm -rf .install .check .test .doc
	find modules/rtm/src \( -name \*.mod -o -name \*.o -o -name \*.so \) -delete

.install/devtools: | .install
	+ time Rscript -e "if(!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools', repos = 'http://cran.rstudio.com', Ncpus = ${NCPUS})"
	echo `date` > $@

.install/roxygen2: | .install
	+ time Rscript -e "if(!requireNamespace('roxygen2', quietly = TRUE)) install.packages('roxygen2', repos = 'http://cran.rstudio.com', Ncpus = ${NCPUS})"
	echo `date` > $@

.install/testthat: | .install
	+ time Rscript -e "if(!requireNamespace('testthat', quietly = TRUE)) install.packages('testthat', repos = 'http://cran.rstudio.com', Ncpus = ${NCPUS})"
	echo `date` > $@

.install/mockery: | .install
	+ time Rscript -e "if(!requireNamespace('mockery', quietly = TRUE)) install.packages('mockery', repos = 'http://cran.rstudio.com', Ncpus = ${NCPUS})"
	echo `date` > $@

depends_R_pkg = time Rscript -e "devtools::install_deps('$(strip $(1))', threads = ${NCPUS}, dependencies = TRUE);"
install_R_pkg = time Rscript -e "devtools::install('$(strip $(1))', Ncpus = ${NCPUS});"
check_R_pkg = Rscript scripts/check_with_errors.R $(strip $(1))
test_R_pkg = Rscript -e "devtools::test('"$(strip $(1))"', stop_on_failure = TRUE, stop_on_warning = FALSE)" # TODO: Raise bar to stop_on_warning = TRUE when we can
doc_R_pkg = Rscript -e "devtools::document('"$(strip $(1))"')"

$(ALL_PKGS_I) $(ALL_PKGS_C) $(ALL_PKGS_T) $(ALL_PKGS_D): | .install/devtools .install/roxygen2 .install/testthat

.SECONDEXPANSION:
.doc/%: $$(wildcard %/**/*) $$(wildcard %/*) | $$(@D)
	+ $(call depends_R_pkg, $(subst .doc/,,$@))
	$(call doc_R_pkg, $(subst .doc/,,$@))
	echo `date` > $@

.install/%: $$(wildcard %/**/*) $$(wildcard %/*) .doc/% | $$(@D)
	+ $(call install_R_pkg, $(subst .install/,,$@))
	echo `date` > $@

.check/%: $$(wildcard %/**/*) $$(wildcard %/*) | $$(@D)
	+ $(call check_R_pkg, $(subst .check/,,$@))
	echo `date` > $@

.test/%: $$(wildcard %/**/*) $$(wildcard %/*) | $$(@D)
	$(call test_R_pkg, $(subst .test/,,$@))
	echo `date` > $@

# Install dependencies declared by Shiny apps
.shiny_depends/%: $$(wildcard %/**/*) $$(wildcard %/*) | $$(@D)
	Rscript scripts/install_shiny_deps.R $(subst .shiny_depends/,shiny/,$@)
	echo `date` > $@
