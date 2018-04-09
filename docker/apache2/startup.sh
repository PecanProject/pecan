#!/bin/bash

set -e

if [ -f /etc/configured ]; then
  echo 'already configured'
else
  #code that need to run only one time ....
  update-locale
  date > /etc/configured
fi
