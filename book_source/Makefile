all: build deploy 

build:
	./check_bkd_pkg.R
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")'
	
clean:
	rm -rf ../book/*

deploy:
	./deploy.sh
	
pdf:
	./check_bkd_pkg.R
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::pdf_book")'
