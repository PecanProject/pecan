#!/bin/bash

NEW_VERSION=1.9.0

for x in $(find -name DESCRIPTION); do
  sed -i"" "s/Version: \(.*\).9000/Version: \1/" $x
done

CHANGED="$(for x in $(find . -name DESCRIPTION); do git diff --name-only v1.8.0 HEAD -- $(dirname $x)/R; done | sed 's#/R/.*##' | sort -u)"
for x in ${CHANGED}; do
  sed -i"" "s/Version: .*/Version: ${NEW_VERSION}/" $x/DESCRIPTION
done
