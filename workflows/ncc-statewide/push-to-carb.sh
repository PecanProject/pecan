#!/usr/bin/env bash

aws s3 sync --profile ccmmf \
  /projectnb/dietzelab/ccmmf/management/ncc_event_files/ \
  s3://carb/management/ncc/v1.0/
