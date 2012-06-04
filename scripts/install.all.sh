for f in common utils db modules/meta.analysis models/ed; do
  R CMD build $f && R CMD INSTALL $f
done
