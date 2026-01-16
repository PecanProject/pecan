#!/bin/bash

# save old state
BRANCH=$(git branch | awk '/^\*/ { print $2}')
git stash -u

# update all remotes
git fetch --all

# update main
git checkout main
git merge upstream/main
git push

# update develop
git checkout develop
git merge upstream/develop
git push

# restore
git checkout ${BRANCH}
git stash pop
