#!/bin/bash

. /build/install_pecan_preprocessor.sh

echo "######################################################################"
echo "PECAN"
echo "######################################################################"
if [ ! -e ${HOME}/pecan ]; then
  cd
  git clone https://github.com/PecanProject/pecan.git
fi
cd ${HOME}/pecan
git pull
make

 curl -o /var/www/html/pecan.pdf https://www.gitbook.com/download/pdf/book/pecan/pecan-documentation
 rm /var/www/html/index.html
 ln -s  ${HOME}/pecan/documentation/index_vm.html /var/www/html/index.html
if [ ! -e ${HOME}/pecan/web/config.php ]; then
  sed -e "s#browndog_url=.*#browndog_url=\"${BROWNDOG_URL}\";#" \
      -e "s#browndog_username=.*#browndog_username=\"${BROWNDOG_USERNAME}\";#" \
      -e "s#browndog_password=.*#browndog_password=\"${BROWNDOG_PASSWORD}\";#" \
      -e "s#googleMapKey=.*#googleMapKey=\"${GOOGLE_MAP_KEY}\";#" \
      -e "s/carya/$USER/g" ${HOME}/pecan/web/config.example.php > ${HOME}/pecan/web/config.php
fi

if [ ! -e ${HTTP_CONF}/pecan.conf ]; then
  cat > /tmp/pecan.conf << EOF
Alias /pecan ${HOME}/pecan/web
<Directory ${HOME}/pecan/web>
  DirectoryIndex index.php
  Options +ExecCGI
  Require all granted
</Directory>
EOF
   cp /tmp/pecan.conf ${HTTP_CONF}/pecan.conf
  rm /tmp/pecan.conf
fi
