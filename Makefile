NCPUS ?= 1

BASE := logger utils db settings visualization qaqc remote workflow

MODELS := basgra biocro clm45 dalec dvmdostem ed fates gday jules linkages \
				ldndc lpjguess maat maespa preles sibcasa sipnet stics template

MODULES := allometry assim.batch assim.sequential benchmark \
				 data.atmosphere data.land data.remote \
				 emulator meta.analysis \
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

SETROPTIONS := "options(Ncpus = ${NCPUS})"

EXPECTED_ROXYGEN_VERSION := 7.3.1
INSTALLED_ROXYGEN_VERSION := $(shell Rscript \
	-e "if (requireNamespace('roxygen2', quietly = TRUE)) {" \
	-e   "cat(as.character(packageVersion('roxygen2')))" \
	-e "}")

### Macros

# Generates a list of all files and subdirectories at any depth inside its argument
recurse_dir = $(foreach d, $(wildcard $1*), $(call recurse_dir, $d/) $d)

# Filters a list from recurse_dir to remove paths that are directories
# Caveat: Really only removes *direct parents* of other paths *in the list*:
# 	$(call drop_dirs,a a/b a/b/c) => 'a/b/c',
# 	but $(call drop_dirs,a a/b d d/e/f) => 'a/b d d/e/f'
# For output from recurse_dir this removes all dirs, but in other cases beware.
drop_parents = $(filter-out $(patsubst %/,%,$(dir $1)), $1)

# Generates a list of regular files at any depth inside its argument
files_in_dir = $(call drop_parents, $(call recurse_dir, $1))

# HACK: NA vs TRUE switch on dependencies argument is an ugly workaround for
# a circular dependency between benchmark and data.land.
# When this is fixed, can go back to simple `dependencies = TRUE`
depends_R_pkg = ./scripts/time.sh "depends ${1}" ./scripts/confirm_deps.R ${1} \
	$(if $(findstring modules/benchmark,$(1)),NA,TRUE)
install_R_pkg = ./scripts/time.sh "install ${1}" Rscript \
	-e ${SETROPTIONS} \
	-e "remotes::install_local('$(strip $(1))', force=TRUE, dependencies=FALSE, upgrade=FALSE)"
check_R_pkg = ./scripts/time.sh "check ${1}" Rscript scripts/check_with_errors.R $(strip $(1))
test_R_pkg = ./scripts/time.sh "test ${1}" Rscript \
	-e "devtools::test('$(strip $(1))'," \
	-e "stop_on_failure = TRUE," \
	-e "stop_on_warning = FALSE)" # TODO: Raise bar to stop_on_warning = TRUE when we can

doc_R_pkg = \
	$(if \
		$(filter ${EXPECTED_ROXYGEN_VERSION},${INSTALLED_ROXYGEN_VERSION}), \
		./scripts/time.sh "document ${1}" \
			Rscript -e "devtools::document('"$(strip $(1))"')", \
		$(error Roxygen2 version is ${INSTALLED_ROXYGEN_VERSION}, \
			but PEcAn package documentation must be built with exactly \
			version ${EXPECTED_ROXYGEN_VERSION}))


depends = .doc/$(1) .install/$(1) .check/$(1) .test/$(1)


### Rules

.PHONY: all install check test document shiny \
            check_base check_models check_modules 

all: install document


check_base: $(BASE_C) 
check_models: $(MODELS_C) 

# Install base first as Modules has a circular dependency on base,
# and then run a check on modules
check_modules: $(BASE_I) $(MODULES_C) 

document: $(ALL_PKGS_D) .doc/base/all
install: $(ALL_PKGS_I) .install/base/all
check: $(ALL_PKGS_C) .check/base/all
test: $(ALL_PKGS_T) .test/base/all
shiny: $(SHINY_I)

# Render the PEcAn bookdown documentation
book:
	cd ./book_source && make build

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

clean:
	rm -rf .install .check .test .doc
	find modules/rtm/src \( -name \*.mod -o -name \*.o -o -name \*.so \) -delete
	find models/basgra/src \( -name \*.mod -o -name \*.o -o -name \*.so \) -delete

.install/devtools: | .install
	+ ./scripts/time.sh "devtools ${1}" Rscript -e ${SETROPTIONS} -e "if(!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools')"
	echo `date` > $@

.install/roxygen2: | .install .install/devtools
	+ ./scripts/time.sh "roxygen2 ${1}" Rscript -e ${SETROPTIONS} \
		-e "if (!requireNamespace('roxygen2', quietly = TRUE)" \
		-e "    || packageVersion('roxygen2') != '"${EXPECTED_ROXYGEN_VERSION}"') {" \
		-e "  cran <- c(getOption('repos'), 'cloud.r-project.org')" \
		-e "  remotes::install_version('roxygen2', '"${EXPECTED_ROXYGEN_VERSION}"', repos = cran, upgrade = FALSE)" \
		-e "}"
	$(eval INSTALLED_ROXYGEN_VERSION := ${EXPECTED_ROXYGEN_VERSION})
	echo `date` > $@

.install/testthat: | .install
	+ ./scripts/time.sh "testthat ${1}" Rscript -e ${SETROPTIONS} -e "if(!requireNamespace('testthat', quietly = TRUE)) install.packages('testthat')"
	echo `date` > $@

.install/mockery: | .install
	+ ./scripts/time.sh "mockery ${1}" Rscript -e ${SETROPTIONS} -e "if(!requireNamespace('mockery', quietly = TRUE)) install.packages('mockery')"
	echo `date` > $@

$(ALL_PKGS_I) $(ALL_PKGS_C) $(ALL_PKGS_T) $(ALL_PKGS_D): | .install/devtools .install/roxygen2 .install/testthat

.SECONDEXPANSION:
.doc/%: $$(call files_in_dir, %) | $$(@D)
	+ $(call depends_R_pkg, $(subst .doc/,,$@))
	$(call doc_R_pkg, $(subst .doc/,,$@))
	echo `date` > $@

.install/%: $$(call files_in_dir, %) .doc/% | $$(@D)
	+ $(call install_R_pkg, $(subst .install/,,$@))
	echo `date` > $@

.check/%: $$(call files_in_dir, %) | $$(@D)
	+ $(call check_R_pkg, $(subst .check/,,$@))
	echo `date` > $@

.test/%: $$(call files_in_dir, %) | $$(@D)
	$(call test_R_pkg, $(subst .test/,,$@))
	echo `date` > $@

# Install dependencies declared by Shiny apps
.shiny_depends/%: $$(call files_in_dir, %) | $$(@D)
	Rscript scripts/install_shiny_deps.R $(subst .shiny_depends/,shiny/,$@)
	echo `date` > $@
