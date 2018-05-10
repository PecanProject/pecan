all: bkdcheck build deploy 

bkdcheck:
	./check_bkd_pkg.R

build: 
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")'
	
clean:
	rm -rf ../book/*

deploy:
	./deploy.sh
	
pdf:
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::pdf_book")'
