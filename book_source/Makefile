.PHONY: all bkdcheck build clean deploy

all: bkdcheck build deploy

bkdcheck:
	./check_bkd_pkg.R

# ANS: This is a hack to get around the fact that because the Demos
# are symlinked into the book_source directory, their relative paths
# to figures break. This gets around that by copying any external
# files from tutorials in the `documentation/tutorials` folder into
# the `book_source/extfiles` folder. That way, `extfiles/image.jpg`
# works in both the original Rmd file and the compiled book.
#
# Any other demos outside the `book_source` folder relying on external
# files will have to use a similar hack. The convention is to stick
# any external files into the `extfiles` folder in the same directory
# as the Rmd root, then create a new variable like the one here and
# add a line like `cp -f` below to copy the files as necessary.

DEMO_1_FIGS := $(wildcard ../documentation/tutorials/01_Demo_Basic_Run/extfiles/*)

build: bkdcheck
	mkdir -p extfiles
	cp -f ${DEMO_1_FIGS} extfiles/
	# options call is a workaround for a behavior change and probable bug in bookdown 0.20:
	# https://stackoverflow.com/a/62583304
	# Remove when this is fixed in Bookdown
	Rscript -e 'options(bookdown.render.file_scope=FALSE); bookdown::render_book("index.Rmd", "bookdown::gitbook")'

clean:
	rm -rf ../book/*

deploy: build
	./deploy.sh

pdf: bkdcheck
	Rscript -e 'options(bookdown.render.file_scope=FALSE); bookdown::render_book("index.Rmd", "bookdown::pdf_book")'
