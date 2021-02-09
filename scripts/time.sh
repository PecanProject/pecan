#!/bin/bash

set -e

TITLE="$(echo $1 | xargs)"
shift

if [ "$TRAVIS" == "true" ]; then
	FOLD_NAME=$( echo "make_$TITLE" | sed -e 's#[^A-Za-z0-9]#_#g' )

    . $( dirname $0 )/travis/func.sh

    travis_time_start "${FOLD_NAME}" "${FOLD_NAME}"
    "$@"
    travis_time_end
elif [ -n "$GITHUB_WORKFLOW" ]; then
    echo "::group::${TITLE}"
    "$@"
    echo "::endgroup::"
else
	echo "=========== START $TITLE ==========="
    time "$@"
	echo "=========== END   $TITLE ==========="
fi
