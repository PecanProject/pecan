#!/bin/bash

TRAVIS_STACK=()
if [ "$(uname -s)" == "Darwin" ]; then
    DATE_OPTION="+%s"
    DATE_DIV=1
else
    DATE_OPTION="+%s%N"
    DATE_DIV=1000000000
fi

function travis_time_start {
    old_setting=${-//[^x]/}
    set +x
    TRAVIS_START_TIME=$(date ${DATE_OPTION})
    TRAVIS_TIME_ID=$( uuidgen | sed 's/-//g' | cut -c 1-8 )
    TRAVIS_FOLD_NAME=$1
    TRAVIS_STACK=("${TRAVIS_FOLD_NAME}#${TRAVIS_TIME_ID}#${TRAVIS_START_TIME}" "${TRAVIS_STACK[@]}")
    echo -e "\e[0Ktravis_fold:start:$TRAVIS_FOLD_NAME"
    echo -e "\e[0Ktravis_time:start:$TRAVIS_TIME_ID"
    if [ "$2" != "" ]; then
        echo "$2"
    fi
    if [[ -n "$old_setting" ]]; then set -x; else set +x; fi
}

function travis_time_end {
    old_setting=${-//[^x]/}
    set +x
    _COLOR=${1:-32}
    TRAVIS_ITEM="${TRAVIS_STACK[0]}"
    TRAVIS_ITEMS=(${TRAVIS_ITEM//#/ })
    TRAVIS_FOLD_NAME="${TRAVIS_ITEMS[0]}"
    TRAVIS_TIME_ID="${TRAVIS_ITEMS[1]}"
    TRAVIS_START_TIME="${TRAVIS_ITEMS[2]}"
    TRAVIS_STACK=("${TRAVIS_STACK[@]:1}")
    TRAVIS_END_TIME=$(date ${DATE_OPTION})
    TIME_ELAPSED_SECONDS=$(( ($TRAVIS_END_TIME - $TRAVIS_START_TIME)/1000000000 ))
    echo -e "travis_time:end:$TRAVIS_TIME_ID:start=$TRAVIS_START_TIME,finish=$TRAVIS_END_TIME,duration=$(($TRAVIS_END_TIME - $TRAVIS_START_TIME))\n\e[0K"
    echo -e "travis_fold:end:$TRAVIS_FOLD_NAME"
    echo -e "\e[0K\e[${_COLOR}mFunction $TRAVIS_FOLD_NAME takes $(( $TIME_ELAPSED_SECONDS / 60 )) min $(( $TIME_ELAPSED_SECONDS % 60 )) sec\e[0m"
    if [[ -n "$old_setting" ]]; then set -x; else set +x; fi
}
