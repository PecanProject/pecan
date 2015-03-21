require(devtools)
#install_github('nimble-dev/nimble/packages/nimble@devel',
#               args="--enable-lib=true")
install.packages("~/Downloads/nimble_0.3-1.tar.gz", repos=NULL, type="source",
                 configure.args="--enable-lib=true")
