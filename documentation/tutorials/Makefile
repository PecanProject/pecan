SOURCES := $(shell cat buildfiles)

RMDS := ${SOURCES:%=%.Rmd}
TARGETS=${SOURCES:%=%.html}

.PHONY: build deploy

all: build

deploy: build
	./deploy.sh

build: ${TARGETS}

%.html: %.Rmd
	Rscript -e "rmarkdown::render('$^')"
