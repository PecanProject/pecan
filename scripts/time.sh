#!/bin/bash

set -e

TITLE="$(echo $1 | xargs)"
shift

if [ -n "$GITHUB_WORKFLOW" ]; then
    echo "::group::${TITLE}"
    "$@"
    echo "::endgroup::"
else
    echo "=========== START $TITLE ==========="
    time "$@"
    echo "=========== END   $TITLE ==========="
fi
