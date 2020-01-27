NCPUS ?= 1

BASE := logger utils db settings visualization qaqc remote workflow

MODELS := basgra biocro clm45 dalec dvmdostem ed fates gday jules linkages \
				lpjguess maat maespa preles sipnet stics template

MODULES := allometry assim.batch assim.sequential benchmark \
				 data.atmosphere data.hydrology data.land \
				 data.remote emulator meta.analysis \
				 photosynthesis priors rtm uncertainty

# Components not currently included in the build
# (Most need more development first)
# 	models: cable
#	modules: data.mining, DART

SHINY := $(dir $(wildcard shiny/*/.))
SHINY := $(SHINY:%/=%)

BASE := $(BASE:%=base/%)
MODELS := $(MODELS:%=models/%)
MODULES := $(MODULES:%=modules/%)
ALL_PKGS := $(BASE) $(MODULES) $(MODELS)

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

# Render the PEcAn bookdown documentation
book:
	cd ./book_source && make build

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

include Makefile.depends

SETROPTIONS = "options(Ncpus = ${NCPUS}, repos = 'https://cran.rstudio.com')"

clean:
	rm -rf .install .check .test .doc
	find modules/rtm/src \( -name \*.mod -o -name \*.o -o -name \*.so \) -delete

.install/devtools: | .install
	+ ./scripts/time.sh "${1}" Rscript -e ${SETROPTIONS} -e "if(!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools')"
	echo `date` > $@

.install/roxygen2: | .install
	+ ./scripts/time.sh "${1}" Rscript -e ${SETROPTIONS} -e "if(!requireNamespace('roxygen2', quietly = TRUE)) install.packages('roxygen2')"
	echo `date` > $@

.install/testthat: | .install
	+ ./scripts/time.sh "${1}" Rscript -e ${SETROPTIONS} -e "if(!requireNamespace('testthat', quietly = TRUE)) install.packages('testthat')"
	echo `date` > $@

.install/mockery: | .install
	+ ./scripts/time.sh "${1}" Rscript -e ${SETROPTIONS} -e "if(!requireNamespace('mockery', quietly = TRUE)) install.packages('mockery')"
	echo `date` > $@

# HACK: assigning to `deps` is an ugly workaround for circular dependencies in utils pkg.
# When these are fixed, can go back to simple `dependencies = TRUE`
depends_R_pkg = ./scripts/time.sh "${1}" Rscript -e ${SETROPTIONS} \
	-e "deps <- if (grepl('base/utils', '$(1)')) { c('Depends', 'Imports', 'LinkingTo') } else { TRUE }" \
	-e "devtools::install_deps('$(strip $(1))', dependencies = deps, upgrade=FALSE)"
install_R_pkg = ./scripts/time.sh "${1}" Rscript -e ${SETROPTIONS} -e "devtools::install('$(strip $(1))', upgrade=FALSE)"
check_R_pkg = ./scripts/time.sh "${1}" Rscript scripts/check_with_errors.R $(strip $(1))

# Would use devtools::test(), but devtools 2.2.1 hardcodes stop_on_failure=FALSE
# To work around this, we reimplement about half of test() here :(
test_R_pkg = ./scripts/time.sh "${1}" Rscript \
	-e "if (length(list.files('$(strip $(1))/tests/testthat', 'test.*.[rR]')) == 0) {" \
	-e		"print('No tests found'); quit('no') }" \
	-e "env <- devtools::load_all('$(strip $(1))', quiet = TRUE)[['env']]" \
	-e "testthat::test_dir('$(strip $(1))/tests/testthat', env = env," \
	-e 		"stop_on_failure = TRUE, stop_on_warning = FALSE)" # TODO: Raise bar to stop_on_warning = TRUE when we can

doc_R_pkg = ./scripts/time.sh "${1}" Rscript -e "devtools::document('"$(strip $(1))"')"

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
