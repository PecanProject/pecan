#!/bin/bash

# Triggers an incremental Travis build of PEcAn:
# Installs as many dependencies as it can build in 30 minutes,
# then exits with status 0 so that Travis will cache the built packages.

# Usage:
#	* first find your Travis token and store in an env var named TRAVIS_TOKEN
#		I looked mine up using the Travis CLI tool:
#			travis login --org --auto
#			travis token
#			export TRAVIS_TOKEN=<value from line above>
#	* ./prime_travis_cache.sh [user] [branchname]
# 	* Repeat the call several times (but let each run finish first!)
# 		until cache_buildup.sh reports "build finished"

USER=${1:-PecanProject}
BRANCH=${2:-develop}

BODY='{
  "request": {
    "message":"Prime caches via API",
    "branch":"'${BRANCH}'",
    "config": {
      "install":"echo skipping",
      "before_script":"echo skipping",
      "script":"scripts/travis/cache_buildup.sh 30m"}
}}'

curl -s -X POST \
 -H "Content-Type: application/json" \
 -H "Accept: application/json" \
 -H "Travis-API-Version: 3" \
 -H "Authorization: token ${TRAVIS_TOKEN}" \
 -d "${BODY}" \
 https://api.travis-ci.org/repo/${USER}%2Fpecan/requests
