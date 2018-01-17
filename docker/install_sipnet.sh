

. /build/install_pecan_preprocessor.sh

echo "######################################################################"
echo "SIPNET"
echo "######################################################################"
if [ ! -e ${HOME}/sipnet_unk ]; then
  cd
  curl -o sipnet_unk.tar.gz http://isda.ncsa.illinois.edu/~kooper/PEcAn/models/sipnet_unk.tar.gz
  tar zxf sipnet_unk.tar.gz
  rm sipnet_unk.tar.gz
fi
cd ${HOME}/sipnet_unk/
make clean
make
cp sipnet /usr/local/bin/sipnet.runk
make clean

if [ ! -e ${HOME}/sipnet_r136 ]; then
  cd
  curl -o sipnet_r136.tar.gz http://isda.ncsa.illinois.edu/~kooper/EBI/sipnet_r136.tar.gz
  tar zxf sipnet_r136.tar.gz
  rm sipnet_r136.tar.gz
  sed -i 's#$(LD) $(LIBLINKS) \(.*\)#$(LD) \1 $(LIBLINKS)#' ${HOME}/sipnet_r136/Makefile
fi
cd ${HOME}/sipnet_r136/
make clean
make
cp sipnet /usr/local/bin/sipnet.r136
make clean
