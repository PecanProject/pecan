all: build deploy 

build: 
	Rscript -e 'bookdown::render_book("index.Rmd","bookdown::gitbook", output_dir = "../book")'
	
clean:
	rm -rf ../book/*

deploy:
	./deploy.sh
	
pdf:
	Rscript -e 'bookdown::render_book("index.Rmd","bookdown::pdf_book", output_dir = "../book")'