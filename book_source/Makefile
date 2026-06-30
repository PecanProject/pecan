.PHONY: all bkdcheck build clean

all: bkdcheck build

bkdcheck:
	./check_bkd_pkg.R

build: bkdcheck
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")'

CLEAN_DIRS = _book

clean:
	rm -rf $(CLEAN_DIRS)

pdf: bkdcheck
	Rscript -e 'options(bookdown.render.file_scope=FALSE); bookdown::render_book("index.Rmd", "bookdown::pdf_book")'
