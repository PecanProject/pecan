.PHONY: all bkdcheck build clean deploy

all: bkdcheck build deploy

bkdcheck:
	./check_bkd_pkg.R

# ANS: This is a hack to get around the fact that the because the
# Demos are symlinked into the book_source directory, their relative
# paths to figures break. This gets around that by symbolically
# linking the figures in the `documentation/tutorials` folder into the
# `book_source/figures` folder. That way, `figures/image.jpg` works in
# both the original Rmd file and the compiled book.
#
# Any other demos outside the `book_source` folder relying on external
# files will have to use a similar hack. The convention is to stick
# any external files into the `figures` folder in the same directory
# as the Rmd root, then create a new variable like the one here and
# add a line like `ln -fs` below to create the appropriate link.
#
# Note that an extra "../" has to be added to the variable path
# because the wildcard is relative to the current directory while the
# link itself is relative to the `figures` path, which is one
# directory lower.

DEMO_1_FIGS := $(patsubst %,../%,$(wildcard ../documentation/tutorials/01_Demo_Basic_Run/figures/*))

build: bkdcheck
	ln -fs ${DEMO_1_FIGS} figures/
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")'

clean:
	rm -rf ../book/*

deploy: build
	./deploy.sh

pdf: bkdcheck
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::pdf_book")'
