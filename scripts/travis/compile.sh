#!/bin/bash

# TODO: Would probably be faster to use -j2 NCPUS=1 as for other steps
# but many dependency compilations seem not parallel-safe
NCPUS=1

for x in base/logger base/remote modules/emulator base/utils base/db base/settings base/visualization base/qaqc modules/data.atmosphere modules/data.land modules/priors modules/uncertainty base/workflow modules/allometry modules/meta.analysis modules/assim.batch modules/assim.sequential modules/benchmark modules/data.hydrology modules/data.remote modules/photosynthesis models/template models/ed modules/rtm models/biocro models/clm45 models/dalec models/dvmdostem models/fates models/gday models/jules models/linkages models/lpjguess models/maat models/maespa models/preles models/sipnet base/all ; do
    key=$(echo "make/$x" | sed 's#/#_#g')
    if [ "$x" == "base/utils" ]; then
        deps="c('Depends', 'Imports', 'LinkingTo')"
    else
        deps="TRUE"
    fi

    (
        travis_time_start "${key}" "building PECAN [${x}]"
        Rscript -e "devtools::install_deps('${x}', Ncpus = ${NCPUS}, dependencies = )"
        Rscript -e "devtools::document('$x');"
        Rscript -e "devtools::install('$x', Ncpus = ${NCPUS});"
        Rscript -e "devtools::test('$x', stop_on_failure = TRUE, stop_on_warning = FALSE);"
        REBUILD_DOCS=FALSE Rscript scripts/check_with_errors.R "${x}"
        travis_time_end
    )
done
