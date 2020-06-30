#!/bin/bash

cd R; ./entrypoint.R & 
PID=$!

while ! curl --output /dev/null --silent http://localhost:8000
do 
  sleep 1 && echo -n .
done

cd ../tests; ./alltests.R
kill $PID