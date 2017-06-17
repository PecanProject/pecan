#!/bin/sh

exec chpst -u root /usr/sbin/apache2 -DFOREGROUND off 2>&1
