for f in common utils db modules/meta.analysis; do
  R CMD build $f && R CMD INSTALL $f
done
