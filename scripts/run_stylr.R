#!/usr/bin/env Rscript

# Author: Ken Youens-Clark <kyclark@arizona.edu>
# Date: 1 June 2020
# Purpose: Run "stylr" on R programs to reformat code

suppressMessages(library("optparse"))
suppressMessages(library("R.utils"))
suppressMessages(library("styler"))

option_list = list(
  make_option(
    c("-f", "--file"),
    default = "",
    type = "character",
    help = "Input file",
    metavar = "file"
  ),
  make_option(
    c("-d", "--dir"),
    default = "",
    type = "character",
    help = "Input directory",
    metavar = "dir"
  )
)

opt.parser = OptionParser(option_list = option_list);
opt = parse_args(opt.parser);

files = if ((opt$file != "") & file.exists(opt$file)) {
    c(opt$file)
} else {
    list.files(path = opt$dir, pattern = ".R$", recursive = TRUE, full.names = TRUE)
}
num_files = length(files)

if (num_files == 0) {
  stop(paste("Found no *.R files in ", opt$dir))
} else {
  printf("I found %s file%s\n", num_files, if (num_files==1) '' else 's')
}

for (file in files) {
    printf("Processing '%s'\n", file)
    #file.copy(file, paste0(file, '.orig'))
    style_file(file, style = tidyverse_style, scope = "tokens", strict = TRUE)
    break
}

print('Done.')
