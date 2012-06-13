for f in all common utils db modules/meta.analysis modules/uncertainty models/ed
do
  R CMD build $f && R CMD INSTALL $f
done
