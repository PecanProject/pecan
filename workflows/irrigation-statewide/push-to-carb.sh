#!/usr/bin/env bash

aws s3 sync --profile ccmmf \
  /projectnb/dietzelab/ccmmf/management/irrigation_event_files/ \
  s3://carb/management/irrigation/v1.0/
