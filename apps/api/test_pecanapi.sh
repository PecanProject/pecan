#!/bin/bash

cd R; ./entrypoint.R 2>/dev/null &
PID=$!

while ! curl --output /dev/null --silent http://localhost:8000
do 
  sleep 1 && echo -n .
done

cd ../tests; ./alltests.R
kill $PID