download_libs() {
# On debian CI we want to test against system libv8
if [ "$USER" = "salsaci" ]; then
  return;
fi

# Gets the R target architecture in case of qemu-containers, e.g
# https://hub.docker.com/r/i386/debian
# Which reports uname -m: x86_64 (only i386 seems to have this issue)
RARCH=$(${R_HOME}/bin/Rscript -e 'cat(R.Version()$arch)')
case $RARCH in
  x86_64 | arm64 | aarch64)
    echo "Target architecture: $RARCH"
    ;;
  *)
  echo "Unexpected architecture: $RARCH"
  return;
  ;;
esac

# RHDT compilers are using an older libc++
# https://github.com/jeroen/V8/issues/137
if test -f "/etc/redhat-release" && grep -Fq "release 7" "/etc/redhat-release"; then
IS_CENTOS7=1
fi

IS_MUSL=$(ldd --version 2>&1 | grep musl)
if [ $? -eq 0 ] && [ "$IS_MUSL" ]; then
  URL="https://github.com/jeroen/V8/releases/download/v3.6.0/v8-9.6.180.12-alpine.tar.gz"
elif [ "$RARCH" = "arm64" ] || [ "$RARCH" = "aarch64" ]; then
  URL="https://github.com/jeroen/V8/releases/download/v3.6.0/v8-9.6.180.12-arm64.tar.gz"
else
  IS_GCC4=$($CXX --version | grep -P '^g++.*[^\d.]4(\.\d){2}')
  if [ $? -eq 0 ] && [ "$IS_GCC4" ]; then
    URL="https://github.com/jeroen/V8/releases/download/v3.6.0/v8-6.8.275.32-gcc-4.8.tar.gz"
  elif [ "$IS_CENTOS7" ]; then
    URL="https://github.com/jeroen/V8/releases/download/v3.6.0/v8-6.8.275.32-gcc-4.8.tar.gz"
  else
    URL="https://github.com/jeroen/V8/releases/download/v3.6.0/v8-9.6.180.12-amd64.tar.gz"
  fi
fi
if [ ! -f ".deps/lib/libv8_monolith.a" ]; then
  ${R_HOME}/bin/R -q -e "curl::curl_download('$URL','libv8.tar.gz',quiet=FALSE)"
  tar xzf libv8.tar.gz
  rm -f libv8.tar.gz
  mv v8 .deps
fi
PKG_CFLAGS="-I${PWD}/.deps/include"
PKG_LIBS="-L${PWD}/.deps/lib -lv8_monolith"
}

download_libs

