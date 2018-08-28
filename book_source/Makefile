.PHONY: all bkdcheck build clean deploy

all: bkdcheck build deploy

bkdcheck:
	./check_bkd_pkg.R

build: bkdcheck
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")'
	
clean:
	rm -rf ../book/*

deploy: build
	./deploy.sh
	
pdf: bkdcheck
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::pdf_book")'
