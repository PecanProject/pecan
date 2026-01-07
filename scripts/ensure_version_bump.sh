#!/bin/bash

set -e

# Check for PEcAn packages that need version increments
#
# Sample usage: ./ensure_version_bump.sh v1.9.0
#
# Enforces a change in version number for every PEcAn package that has changed
#	since the last tagged release.
# Output: silent if all changed packages have new versions,
#	else prints a list of packages that need updating and gives instructions
#	to update them.
# Exit status: 0 if all packages OK, 1 if updates needed
#
# Note that it only enforces that the version _differs_ from the release:
# * It does not catch version decreases. Don't do that!
# * You can decide what version to bump to.
# The output recommends adding '.9000' to the existing version (the "dev mode"
#	convention), but this is not enforced and it is OK to have multiple updates
#	per release cycle. In particular, in the runup to a PEcAn release this
#	allows bumping package versions one at a time as we complete their
#	prerelease chores (changelog curation, semver check, etc) rather than
#	needing to bump all packages in a single commit.

# Git tag (or hash or other ref) to compare against.
# If not given as an argument, defaults to the most recently created tag.
git_ref=${1:-$(git tag --sort=-creatordate | head -n1)}

# Directory inside which to check every R package.
# If not given as an argument, defaults to the current working directory.
check_dir=${2:-.}


pkg_changed() {
	! git diff --quiet "$git_ref" -- "${1}"
}
version_changed() {
	git diff -U0 "$git_ref" -- "${1}"/DESCRIPTION | grep -q '^+Version: '
}


pkgdirs=$(find "$check_dir" -name DESCRIPTION | xargs dirname | sort)

while IFS= read -r pkg; do
	if pkg_changed "$pkg" && ! version_changed "$pkg"; then
		export unbumped="$unbumped"" $pkg"
	fi
done <<< "$pkgdirs"

if [[ -n "$unbumped" ]]; then
	echo "These packages have changed since $git_ref and need a version increment:"
	echo "    $unbumped"
	echo "Edit the DESCRIPTION file(s) to add '.9000' to the 'Version:' field."
	exit 1
fi
