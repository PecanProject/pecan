#!/bin/bash

# compile base/logger
(
    travis_time_start "make_base_logger" "building PECAN [base/logger]"
    Rscript -e "devtools::install_deps('base/logger', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile base/remote
(
    travis_time_start "make_base_remote" "building PECAN [base/remote]"
    Rscript -e "devtools::install_deps('base/remote', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile modules/emulator
(
    travis_time_start "make_modules_emulator" "building PECAN [modules/emulator]"
    Rscript -e "devtools::install_deps('modules/emulator', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile base/utils
(
    travis_time_start "make_base_utils" "building PECAN [base/utils]"
    Rscript -e "devtools::install_deps('base/utils', Ncpus = 1, dependencies = c('Depends', 'Imports', 'LinkingTo'))"
    travis_time_end
)

# compile base/db
(
    travis_time_start "make_base_db" "building PECAN [base/db]"
    Rscript -e "devtools::install_deps('base/db', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile base/settings
(
    travis_time_start "make_base_settings" "building PECAN [base/settings]"
    Rscript -e "devtools::install_deps('base/settings', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile base/visualization
(
    travis_time_start "make_base_visualization" "building PECAN [base/visualization]"
    Rscript -e "devtools::install_deps('base/visualization', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile base/qaqc
(
    travis_time_start "make_base_qaqc" "building PECAN [base/qaqc]"
    Rscript -e "devtools::install_deps('base/qaqc', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile modules/data.atmosphere
(
    travis_time_start "make_modules_data.atmosphere" "building PECAN [modules/data.atmosphere]"
    Rscript -e "devtools::install_deps('modules/data.atmosphere', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile modules/data.land
(
    travis_time_start "make_modules_data.land" "building PECAN [modules/data.land]"
    Rscript -e "devtools::install_deps('modules/data.land', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile modules/priors
(
    travis_time_start "make_modules_priors" "building PECAN [modules/priors]"
    Rscript -e "devtools::install_deps('modules/priors', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile modules/uncertainty
(
    travis_time_start "make_modules_uncertainty" "building PECAN [modules/uncertainty]"
    Rscript -e "devtools::install_deps('modules/uncertainty', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile base/workflow
(
    travis_time_start "make_base_workflow" "building PECAN [base/workflow]"
    Rscript -e "devtools::install_deps('base/workflow', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile modules/allometry
(
    travis_time_start "make_modules_allometry" "building PECAN [modules/allometry]"
    Rscript -e "devtools::install_deps('modules/allometry', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile modules/meta.analysis
(
    travis_time_start "make_modules_meta.analysis" "building PECAN [modules/meta.analysis]"
    Rscript -e "devtools::install_deps('modules/meta.analysis', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile modules/assim.batch
(
    travis_time_start "make_modules_assim.batch" "building PECAN [modules/assim.batch]"
    Rscript -e "devtools::install_deps('modules/assim.batch', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile modules/assim.sequential
(
    travis_time_start "make_modules_assim.sequential" "building PECAN [modules/assim.sequential]"
    Rscript -e "devtools::install_deps('modules/assim.sequential', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile modules/benchmark
(
    travis_time_start "make_modules_benchmark" "building PECAN [modules/benchmark]"
    Rscript -e "devtools::install_deps('modules/benchmark', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile modules/data.hydrology
(
    travis_time_start "make_modules_data.hydrology" "building PECAN [modules/data.hydrology]"
    Rscript -e "devtools::install_deps('modules/data.hydrology', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile modules/data.remote
(
    travis_time_start "make_modules_data.remote" "building PECAN [modules/data.remote]"
    Rscript -e "devtools::install_deps('modules/data.remote', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile modules/photosynthesis
(
    travis_time_start "make_modules_photosynthesis" "building PECAN [modules/photosynthesis]"
    Rscript -e "devtools::install_deps('modules/photosynthesis', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile models/template
(
    travis_time_start "make_models_template" "building PECAN [models/template]"
    Rscript -e "devtools::install_deps('models/template', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile models/ed
(
    travis_time_start "make_models_ed" "building PECAN [models/ed]"
    Rscript -e "devtools::install_deps('models/ed', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile modules/rtm
(
    travis_time_start "make_modules_rtm" "building PECAN [modules/rtm]"
    Rscript -e "devtools::install_deps('modules/rtm', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile models/biocro
(
    travis_time_start "make_models_biocro" "building PECAN [models/biocro]"
    Rscript -e "devtools::install_deps('models/biocro', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile models/clm45
(
    travis_time_start "make_models_clm45" "building PECAN [models/clm45]"
    Rscript -e "devtools::install_deps('models/clm45', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile models/dalec
(
    travis_time_start "make_models_dalec" "building PECAN [models/dalec]"
    Rscript -e "devtools::install_deps('models/dalec', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile models/dvmdostem
(
    travis_time_start "make_models_dvmdostem" "building PECAN [models/dvmdostem]"
    Rscript -e "devtools::install_deps('models/dvmdostem', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile models/fates
(
    travis_time_start "make_models_fates" "building PECAN [models/fates]"
    Rscript -e "devtools::install_deps('models/fates', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile models/gday
(
    travis_time_start "make_models_gday" "building PECAN [models/gday]"
    Rscript -e "devtools::install_deps('models/gday', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile models/jules
(
    travis_time_start "make_models_jules" "building PECAN [models/jules]"
    Rscript -e "devtools::install_deps('models/jules', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile models/linkages
(
    travis_time_start "make_models_linkages" "building PECAN [models/linkages]"
    Rscript -e "devtools::install_deps('models/linkages', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile models/lpjguess
(
    travis_time_start "make_models_lpjguess" "building PECAN [models/lpjguess]"
    Rscript -e "devtools::install_deps('models/lpjguess', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile models/maat
(
    travis_time_start "make_models_maat" "building PECAN [models/maat]"
    Rscript -e "devtools::install_deps('models/maat', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile models/maespa
(
    travis_time_start "make_models_maespa" "building PECAN [models/maespa]"
    Rscript -e "devtools::install_deps('models/maespa', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile models/preles
(
    travis_time_start "make_models_preles" "building PECAN [models/preles]"
    Rscript -e "devtools::install_deps('models/preles', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile models/sipnet
(
    travis_time_start "make_models_sipnet" "building PECAN [models/sipnet]"
    Rscript -e "devtools::install_deps('models/sipnet', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

# compile base/all
(
    travis_time_start "make_base_all" "building PECAN [base/all]"
    Rscript -e "devtools::install_deps('base/all', Ncpus = 1, dependencies = TRUE)"
    travis_time_end
)

