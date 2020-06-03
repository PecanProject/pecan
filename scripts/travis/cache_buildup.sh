#!/bin/bash

# Motivation:
# Installing PEcAn from scratch involves building a *lot* of dependencies
# and takes longer than one (50-minute) Travis run, so we cache packages
# between runs and only update the ones that have changed since last time.
# But the cache is only updated after a successful build, so we need a way
# to populate it before the first run (or to clean up from a later run that
# times out because too many packages need updating).
# This script provides such a bootstrap:
# It runs an incremental build, pausing after less-then-50 minutes to report
# success and let Travis cache what we've built so far. Call it several times
# until all dependencies are in-cache, then restart any automated builds that
# were timing out before.

# Usage via Travis web interface:
# * Select "More options" > "Trigger build"
# * Choose branch
# * Enter a commit message (I use "cache priming").
#	NB this commit is only visible on Travis and *will not* be pushed to GitHub
# * Enter config as JSON. I use:
#	```
#	install: scripts/travis/cache_buildup.sh
#	script: echo 'just caching dependencies'
#	```
#	- Any section you include here *replaces* that section in .travis.yml
#	- All sections of .travis.yml you *don't* override here will run as normal

# Usage via Travis API: see prime_travis_cache.sh

set +e

MAX_TIME=${1:-30m}
CMDS=${2:-'scripts/travis/install.sh && make install'}

# Spends up to $MAX_TIME installing packages, then sends HUP
timeout ${MAX_TIME} bash -c "${CMDS}"

if [[ $? -ne 0 ]]; then
	# Clean up any lock files left from killing install.packages
	# (these packages will be re-freshened in the next priming round)
	#
	# TODO BUGBUG this assumes staged installation (R 3.6 and later)!
	# R <= 3.5 backs up the old version to lockdir, builds new version in place
	# 	==> killing and deleting lockfile leaves broken package.
	# Also per-package locking is optional; check if make uses it reliably
	#
	find $(Rscript -e 'cat(.libPaths())') -path '*/00LOCK-*' -delete
	echo "Still more packages to cache. Please initiate another build."
else
	echo "Build finished. Cache should now be up to date."
fi

exit 0
