#!/bin/bash

TRAVIS_STACK=()

function travis_time_start {
    set +x
    TRAVIS_START_TIME=$(date +%s%N)
    TRAVIS_TIME_ID=$(cat /dev/urandom | tr -dc 'a-z0-9' | fold -w 8 | head -n 1)
    TRAVIS_FOLD_NAME=$1
    TRAVIS_STACK=("${TRAVIS_FOLD_NAME}#${TRAVIS_TIME_ID}#${TRAVIS_START_TIME}" "${TRAVIS_STACK[@]}")
    echo -e "\e[0Ktravis_fold:start:$TRAVIS_FOLD_NAME"
    echo -e "\e[0Ktravis_time:start:$TRAVIS_TIME_ID"
    if [ "$2" != "" ]; then
        echo "$2"
    fi
    set -x
}

function travis_time_end {
    set +x
    _COLOR=${1:-32}
    TRAVIS_ITEM="${TRAVIS_STACK[0]}"
    TRAVIS_ITEMS=(${TRAVIS_ITEM//#/ })
    TRAVIS_FOLD_NAME="${TRAVIS_ITEMS[0]}"
    TRAVIS_TIME_ID="${TRAVIS_ITEMS[1]}"
    TRAVIS_START_TIME="${TRAVIS_ITEMS[2]}"
    TRAVIS_STACK=("${TRAVIS_STACK[@]:1}")
    TRAVIS_END_TIME=$(date +%s%N)
    TIME_ELAPSED_SECONDS=$(( ($TRAVIS_END_TIME - $TRAVIS_ITEMS[2])/1000000000 ))
    echo -e "travis_time:end:$TRAVIS_TIME_ID:start=$TRAVIS_START_TIME,finish=$TRAVIS_END_TIME,duration=$(($TRAVIS_END_TIME - $TRAVIS_START_TIME))\n\e[0K"
    echo -e "travis_fold:end:$TRAVIS_FOLD_NAME"
    echo -e "\e[0K\e[${_COLOR}mFunction $TRAVIS_FOLD_NAME takes $(( $TIME_ELAPSED_SECONDS / 60 )) min $(( $TIME_ELAPSED_SECONDS % 60 )) sec\e[0m"
    set -x
}
