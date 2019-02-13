#!/bin/bash

# TODO: Would probably be faster to use -j2 NCPUS=1 as for other steps
# but many dependency compilations seem not parallel-safe
NCPUS=1

# all packages to install
PAKCAGES=$(cat $(dirname $0)/build_order.txt)
for x in ${PAKCAGES} ; do
    key=$(echo "make/$x" | sed 's#/#_#g')
    if [ "$x" == "base/utils" ]; then
        deps="c('Depends', 'Imports', 'LinkingTo')"
    else
        deps="TRUE"
    fi

    travis_time_start "${key}" "building PECAN [${x}]"

    travis_time_start "${key}_deps" "Installing dependencies"
    Rscript -e "devtools::install_deps('${x}', Ncpus = ${NCPUS}, dependencies = ${deps})"
    travis_time_end

    travis_time_start "${key}_docs" "Roxygen documentation"
    Rscript -e "devtools::document('$x');"
    travis_time_end

    travis_time_start "${key}_install" "Installing package"
    Rscript -e "devtools::install('$x', Ncpus = ${NCPUS});"
    travis_time_end

    travis_time_start "${key}_test" "Testing package"
    Rscript -e "devtools::test('$x', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end

    travis_time_start "${key}_check" "Checking package"
    REBUILD_DOCS=FALSE Rscript scripts/check_with_errors.R "${x}"
    travis_time_end

    travis_time_end
done

