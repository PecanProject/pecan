#!/usr/bin/env Rscript

split_into_batches <- function(x, batch_size) {
  split(x, ceiling(seq_along(x) / batch_size))
}
