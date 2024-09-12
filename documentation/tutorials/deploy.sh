#!/usr/bin/env bash

# Exit on error
set -e

# Check for GITHUB_PAT; don't run if unset
if [ -z "${GITHUB_PAT}" ]; then
    echo "GITHUB_PAT not set. Skipping deploy step."
    exit 0
fi

# Clone book_hosted directory if it doesn't exist
if [ ! -d book_hosted ]; then
    # Set GitHub user
    GH_USER=${TRAVIS_REPO_SLUG%/*}

	# Don't deploy if documentation git repo does not exist
	GH_STATUS=$(curl -s -w %{http_code} -I https://github.com/${GH_USER}/pecan-documentation -o /dev/null)
    if [[ $GH_STATUS != 200 ]]; then
	  echo "Can't find a repository at https://github.com/${GH_USER}/pecan-documentation"
	  echo "Will not render tutorials."
	  exit 0
	fi

    # configure your name and email if you have not done so
    git config --global user.email "pecanproj@gmail.com"
    git config --global user.name "TRAVIS-DOC-BUILD"

    git clone https://${GITHUB_PAT}@github.com/${GH_USER}/pecan-documentation.git book_hosted
fi

# Create tutorial directory
mkdir -p book_hosted/tutorials

# Copy all HTML files into target folder
while read f; do
    cp -r ${f}.html book_hosted/tutorials
done < buildfiles

cd book_hosted
git add --all  *
git commit -m "Update tutorials `date`" || true
git push -q origin latest
