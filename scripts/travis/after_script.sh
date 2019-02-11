#!/bin/bash

set -e
. $( dirname $0 )/func.sh

# BUILD DOCUMENTATION
(
    travis_time_start "build_book" "Building Book"
    cd book_source
    make
    travis_time_end
)

# BUILD TUTORIALS
(
    travis_time_start "build_tutorials" "Building Tutorials"
    cd documentation/tutorials
    make build deploy
    travis_time_end
)
