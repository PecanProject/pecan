#!/usr/bin/env bash

aws s3 sync --profile ccmmf \
  /projectnb/dietzelab/ccmmf/management/fertilization_event_files/ \
  s3://carb/management/fertilization/v1.0/
