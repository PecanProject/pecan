BASE := utils db settings visualization

MODELS := biocro clm45 dalec ed fates gday jules linkages \
				lpjguess maat maespa preles sipnet

MODULES := allometry assim.batch assim.sequential benchmark \
				 data.atmosphere data.hydrology data.land \
				 data.mining data.remote emulator meta.analysis \
				 photosynthesis priors rtm uncertainty

MODELS := $(MODELS:%=models/%)
MODULES := $(MODULES:%=modules/%)
ALL_PKGS := $(BASE) $(MODELS) $(MODULES)

BASE_I := $(BASE:%=.install/%)
MODELS_I := $(MODELS:%=.install/%)
MODULES_I := $(MODULES:%=.install/%)
ALL_PKGS_I := $(BASE_I) $(MODELS_I) $(MODULES_I)

BASE_C := $(BASE:%=.check/%)
MODELS_C := $(MODELS:%=.check/%)
MODULES_C := $(MODULES:%=.check/%)
ALL_PKGS_C := $(BASE_C) $(MODELS_C) $(MODULES_C)

BASE_T := $(BASE:%=.test/%)
MODELS_T := $(MODELS:%=.test/%)
MODULES_T := $(MODULES:%=.test/%)
ALL_PKGS_T := $(BASE_T) $(MODELS_T) $(MODULES_T)

BASE_D := $(BASE:%=.doc/%)
MODELS_D := $(MODELS:%=.doc/%)
MODULES_D := $(MODULES:%=.doc/%)
ALL_PKGS_D := $(BASE_D) $(MODELS_D) $(MODULES_D)

.PHONY: all install check test document

all: document install

document: .doc/all
install: document .install/all 
check: install .check/all
test: install .test/all 

### Dependencies
.doc/all: $(ALL_PKGS_D)
.install/all: $(ALL_PKGS_I)
.check/all: $(ALL_PKGS_C)
.test/all: $(ALL_PKGS_T)

$(MODELS_I): $(MODULES_I)

# These two blocks are redunant for now because dependencies need to be 
# specified separately for installation and documentation. I have some ideas 
# about how to fix this, but it's not a priority. For now, just make sure to 
# add dependencies for any new package into here.

.install/db: .install/utils
.install/settings: .install/utils .install/db
.install/visualization: .install/db
.install/modules/data.atmosphere: .install/utils .install/reddyproc
.install/modules/data.land: .install/db .install/utils
.install/modules/meta.analysis: .install/utils .install/db
.install/modules/priors: .install/utils
.install/modules/rtm: .install/modules/assim.batch

.doc/db: .doc/utils
.doc/settings: .doc/utils .doc/db
.doc/visualization: .doc/db
.doc/modules/data.atmosphere: .doc/utils
.doc/modules/data.land: .doc/db .doc/utils
.doc/modules/meta.analysis: .doc/utils .doc/db
.doc/modules/priors: .doc/utils
.doc/modules/rtm: .doc/modules/assim.batch

clean:
	rm -rf .install .check .test .doc

.install/devtools:
	Rscript -e "req <- require('devtools'); if(!req) install.packages('devtools', repos = 'http://cran.rstudio.com')"
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

