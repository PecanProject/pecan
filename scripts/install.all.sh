for f in common utils db modules/meta.analysis modules/uncertainty models/ed models/sipnet all
do
  R CMD build $f && R CMD INSTALL $f
done
