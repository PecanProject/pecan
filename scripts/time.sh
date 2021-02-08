#!/bin/bash

set -e

FOLD_NAME=$( echo "make_$1" | sed -e 's#[^a-z0-9]#_#g' )
shift

if [ "$TRAVIS" == "true" ]; then
    . $( dirname $0 )/travis/func.sh

    travis_time_start "${FOLD_NAME}" "${FOLD_NAME}"
    "$@"
    travis_time_end
elif [ -n "$GITHUB_WORKFLOW" ]; then
    echo "::group::${FOLD_NAME}"
    "$@"
    echo "::endgroup::"
else
    time "$@"
fi
