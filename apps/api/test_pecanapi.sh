#!/bin/bash

cd R; ./entrypoint.R 2>/dev/null &
PID=$!

while ! curl --output /dev/null --silent http://pecan.localhost/
do 
  sleep 1 && echo -n .
done

cd ../tests; ./alltests.R
kill $PID
