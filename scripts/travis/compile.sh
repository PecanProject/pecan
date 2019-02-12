#!/bin/bash

# compile base/logger
(
    travis_time_start "make_base_logger" "building PECAN [base/logger]"
    Rscript -e "devtools::install_deps('base/logger', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('base/logger');"
    Rscript -e "devtools::install('base/logger', Ncpus = 1);"
    Rscript -e "devtools::test('base/logger', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile base/remote
(
    travis_time_start "make_base_remote" "building PECAN [base/remote]"
    Rscript -e "devtools::install_deps('base/remote', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('base/remote');"
    Rscript -e "devtools::install('base/remote', Ncpus = 1);"
    Rscript -e "devtools::test('base/remote', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile modules/emulator
(
    travis_time_start "make_modules_emulator" "building PECAN [modules/emulator]"
    Rscript -e "devtools::install_deps('modules/emulator', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('modules/emulator');"
    Rscript -e "devtools::install('modules/emulator', Ncpus = 1);"
    Rscript -e "devtools::test('modules/emulator', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile base/utils
(
    travis_time_start "make_base_utils" "building PECAN [base/utils]"
    Rscript -e "devtools::install_deps('base/utils', Ncpus = 1, dependencies = c('Depends', 'Imports', 'LinkingTo'))"
    Rscript -e "devtools::document('base/utils');"
    Rscript -e "devtools::install('base/utils', Ncpus = 1);"
    Rscript -e "devtools::test('base/utils', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile base/db
(
    travis_time_start "make_base_db" "building PECAN [base/db]"
    Rscript -e "devtools::install_deps('base/db', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('base/db');"
    Rscript -e "devtools::install('base/db', Ncpus = 1);"
    Rscript -e "devtools::test('base/db', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile base/settings
(
    travis_time_start "make_base_settings" "building PECAN [base/settings]"
    Rscript -e "devtools::install_deps('base/settings', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('base/settings');"
    Rscript -e "devtools::install('base/settings', Ncpus = 1);"
    Rscript -e "devtools::test('base/settings', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile base/visualization
(
    travis_time_start "make_base_visualization" "building PECAN [base/visualization]"
    Rscript -e "devtools::install_deps('base/visualization', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('base/visualization');"
    Rscript -e "devtools::install('base/visualization', Ncpus = 1);"
    Rscript -e "devtools::test('base/visualization', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile base/qaqc
(
    travis_time_start "make_base_qaqc" "building PECAN [base/qaqc]"
    Rscript -e "devtools::install_deps('base/qaqc', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('base/qaqc');"
    Rscript -e "devtools::install('base/qaqc', Ncpus = 1);"
    Rscript -e "devtools::test('base/qaqc', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile modules/data.atmosphere
(
    travis_time_start "make_modules_data.atmosphere" "building PECAN [modules/data.atmosphere]"
    Rscript -e "devtools::install_deps('modules/data.atmosphere', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('modules/data.atmosphere');"
    Rscript -e "devtools::install('modules/data.atmosphere', Ncpus = 1);"
    Rscript -e "devtools::test('modules/data.atmosphere', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile modules/data.land
(
    travis_time_start "make_modules_data.land" "building PECAN [modules/data.land]"
    Rscript -e "devtools::install_deps('modules/data.land', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('modules/data.land');"
    Rscript -e "devtools::install('modules/data.land', Ncpus = 1);"
    Rscript -e "devtools::test('modules/data.land', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile modules/priors
(
    travis_time_start "make_modules_priors" "building PECAN [modules/priors]"
    Rscript -e "devtools::install_deps('modules/priors', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('modules/priors');"
    Rscript -e "devtools::install('modules/priors', Ncpus = 1);"
    Rscript -e "devtools::test('modules/priors', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile modules/uncertainty
(
    travis_time_start "make_modules_uncertainty" "building PECAN [modules/uncertainty]"
    Rscript -e "devtools::install_deps('modules/uncertainty', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('modules/uncertainty');"
    Rscript -e "devtools::install('modules/uncertainty', Ncpus = 1);"
    Rscript -e "devtools::test('modules/uncertainty', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile base/workflow
(
    travis_time_start "make_base_workflow" "building PECAN [base/workflow]"
    Rscript -e "devtools::install_deps('base/workflow', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('base/workflow');"
    Rscript -e "devtools::install('base/workflow', Ncpus = 1);"
    Rscript -e "devtools::test('base/workflow', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile modules/allometry
(
    travis_time_start "make_modules_allometry" "building PECAN [modules/allometry]"
    Rscript -e "devtools::install_deps('modules/allometry', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('modules/allometry');"
    Rscript -e "devtools::install('modules/allometry', Ncpus = 1);"
    Rscript -e "devtools::test('modules/allometry', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile modules/meta.analysis
(
    travis_time_start "make_modules_meta.analysis" "building PECAN [modules/meta.analysis]"
    Rscript -e "devtools::install_deps('modules/meta.analysis', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('modules/meta.analysis');"
    Rscript -e "devtools::install('modules/meta.analysis', Ncpus = 1);"
    Rscript -e "devtools::test('modules/meta.analysis', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile modules/assim.batch
(
    travis_time_start "make_modules_assim.batch" "building PECAN [modules/assim.batch]"
    Rscript -e "devtools::install_deps('modules/assim.batch', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('modules/assim.batch');"
    Rscript -e "devtools::install('modules/assim.batch', Ncpus = 1);"
    Rscript -e "devtools::test('modules/assim.batch', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile modules/assim.sequential
(
    travis_time_start "make_modules_assim.sequential" "building PECAN [modules/assim.sequential]"
    Rscript -e "devtools::install_deps('modules/assim.sequential', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('modules/assim.sequential');"
    Rscript -e "devtools::install('modules/assim.sequential', Ncpus = 1);"
    Rscript -e "devtools::test('modules/assim.sequential', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile modules/benchmark
(
    travis_time_start "make_modules_benchmark" "building PECAN [modules/benchmark]"
    Rscript -e "devtools::install_deps('modules/benchmark', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('modules/benchmark');"
    Rscript -e "devtools::install('modules/benchmark', Ncpus = 1);"
    Rscript -e "devtools::test('modules/benchmark', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile modules/data.hydrology
(
    travis_time_start "make_modules_data.hydrology" "building PECAN [modules/data.hydrology]"
    Rscript -e "devtools::install_deps('modules/data.hydrology', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('modules/data.hydrology');"
    Rscript -e "devtools::install('modules/data.hydrology', Ncpus = 1);"
    Rscript -e "devtools::test('modules/data.hydrology', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile modules/data.remote
(
    travis_time_start "make_modules_data.remote" "building PECAN [modules/data.remote]"
    Rscript -e "devtools::install_deps('modules/data.remote', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('modules/data.remote');"
    Rscript -e "devtools::install('modules/data.remote', Ncpus = 1);"
    Rscript -e "devtools::test('modules/data.remote', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile modules/photosynthesis
(
    travis_time_start "make_modules_photosynthesis" "building PECAN [modules/photosynthesis]"
    Rscript -e "devtools::install_deps('modules/photosynthesis', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('modules/photosynthesis');"
    Rscript -e "devtools::install('modules/photosynthesis', Ncpus = 1);"
    Rscript -e "devtools::test('modules/photosynthesis', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile models/template
(
    travis_time_start "make_models_template" "building PECAN [models/template]"
    Rscript -e "devtools::install_deps('models/template', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('models/template');"
    Rscript -e "devtools::install('models/template', Ncpus = 1);"
    Rscript -e "devtools::test('models/template', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile models/ed
(
    travis_time_start "make_models_ed" "building PECAN [models/ed]"
    Rscript -e "devtools::install_deps('models/ed', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('models/ed');"
    Rscript -e "devtools::install('models/ed', Ncpus = 1);"
    Rscript -e "devtools::test('models/ed', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile modules/rtm
(
    travis_time_start "make_modules_rtm" "building PECAN [modules/rtm]"
    Rscript -e "devtools::install_deps('modules/rtm', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('modules/rtm');"
    Rscript -e "devtools::install('modules/rtm', Ncpus = 1);"
    Rscript -e "devtools::test('modules/rtm', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile models/biocro
(
    travis_time_start "make_models_biocro" "building PECAN [models/biocro]"
    Rscript -e "devtools::install_deps('models/biocro', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('models/biocro');"
    Rscript -e "devtools::install('models/biocro', Ncpus = 1);"
    Rscript -e "devtools::test('models/biocro', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile models/clm45
(
    travis_time_start "make_models_clm45" "building PECAN [models/clm45]"
    Rscript -e "devtools::install_deps('models/clm45', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('models/clm45');"
    Rscript -e "devtools::install('models/clm45', Ncpus = 1);"
    Rscript -e "devtools::test('models/clm45', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile models/dalec
(
    travis_time_start "make_models_dalec" "building PECAN [models/dalec]"
    Rscript -e "devtools::install_deps('models/dalec', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('models/dalec');"
    Rscript -e "devtools::install('models/dalec', Ncpus = 1);"
    Rscript -e "devtools::test('models/dalec', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile models/dvmdostem
(
    travis_time_start "make_models_dvmdostem" "building PECAN [models/dvmdostem]"
    Rscript -e "devtools::install_deps('models/dvmdostem', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('models/dvmdostem');"
    Rscript -e "devtools::install('models/dvmdostem', Ncpus = 1);"
    Rscript -e "devtools::test('models/dvmdostem', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile models/fates
(
    travis_time_start "make_models_fates" "building PECAN [models/fates]"
    Rscript -e "devtools::install_deps('models/fates', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('models/fates');"
    Rscript -e "devtools::install('models/fates', Ncpus = 1);"
    Rscript -e "devtools::test('models/fates', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile models/gday
(
    travis_time_start "make_models_gday" "building PECAN [models/gday]"
    Rscript -e "devtools::install_deps('models/gday', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('models/gday');"
    Rscript -e "devtools::install('models/gday', Ncpus = 1);"
    Rscript -e "devtools::test('models/gday', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile models/jules
(
    travis_time_start "make_models_jules" "building PECAN [models/jules]"
    Rscript -e "devtools::install_deps('models/jules', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('models/jules');"
    Rscript -e "devtools::install('models/jules', Ncpus = 1);"
    Rscript -e "devtools::test('models/jules', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile models/linkages
(
    travis_time_start "make_models_linkages" "building PECAN [models/linkages]"
    Rscript -e "devtools::install_deps('models/linkages', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('models/linkages');"
    Rscript -e "devtools::install('models/linkages', Ncpus = 1);"
    Rscript -e "devtools::test('models/linkages', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile models/lpjguess
(
    travis_time_start "make_models_lpjguess" "building PECAN [models/lpjguess]"
    Rscript -e "devtools::install_deps('models/lpjguess', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('models/lpjguess');"
    Rscript -e "devtools::install('models/lpjguess', Ncpus = 1);"
    Rscript -e "devtools::test('models/lpjguess', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile models/maat
(
    travis_time_start "make_models_maat" "building PECAN [models/maat]"
    Rscript -e "devtools::install_deps('models/maat', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('models/maat');"
    Rscript -e "devtools::install('models/maat', Ncpus = 1);"
    Rscript -e "devtools::test('models/maat', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile models/maespa
(
    travis_time_start "make_models_maespa" "building PECAN [models/maespa]"
    Rscript -e "devtools::install_deps('models/maespa', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('models/maespa');"
    Rscript -e "devtools::install('models/maespa', Ncpus = 1);"
    Rscript -e "devtools::test('models/maespa', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile models/preles
(
    travis_time_start "make_models_preles" "building PECAN [models/preles]"
    Rscript -e "devtools::install_deps('models/preles', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('models/preles');"
    Rscript -e "devtools::install('models/preles', Ncpus = 1);"
    Rscript -e "devtools::test('models/preles', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile models/sipnet
(
    travis_time_start "make_models_sipnet" "building PECAN [models/sipnet]"
    Rscript -e "devtools::install_deps('models/sipnet', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('models/sipnet');"
    Rscript -e "devtools::install('models/sipnet', Ncpus = 1);"
    Rscript -e "devtools::test('models/sipnet', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

# compile base/all
(
    travis_time_start "make_base_all" "building PECAN [base/all]"
    Rscript -e "devtools::install_deps('base/all', Ncpus = 1, dependencies = TRUE)"
    Rscript -e "devtools::document('base/all');"
    Rscript -e "devtools::install('base/all', Ncpus = 1);"
    Rscript -e "devtools::test('base/all', stop_on_failure = TRUE, stop_on_warning = FALSE);"
    travis_time_end
)

