for f in common utils db modules/meta.analysis modules/uncertainty models/ed all
do
  R CMD build $f && R CMD INSTALL $f
done
