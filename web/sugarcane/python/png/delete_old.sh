#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
now=$(date +%s)

for file in "$DIR/"*".png"
do
    if ((($(stat "$file" -c '%Z') + ((60*20))) < $now))
    then
		rm "$file"
    fi
done
