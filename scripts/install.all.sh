for f in common utils db; do
  R CMD build $f && R CMD INSTALL $f
done
