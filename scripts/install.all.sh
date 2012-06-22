for f in common utils db modules/meta.analysis modules/uncertainty modules/rtm models/ed all
do
  R CMD build $f && R CMD INSTALL $f
done
