#!/bin/bash

# compile base/logger
(
    travis_time_start "make_base_logger" "building PECAN [base/logger]"
    devtools::install_deps('base/logger', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile base/remote
(
    travis_time_start "make_base_remote" "building PECAN [base/remote]"
    devtools::install_deps('base/remote', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile modules/emulator
(
    travis_time_start "make_modules_emulator" "building PECAN [modules/emulator]"
    devtools::install_deps('modules/emulator', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile base/utils
(
    travis_time_start "make_base_utils" "building PECAN [base/utils]"
    devtools::install_deps('base/utils', Ncpus = 1, dependencies = c('Depends', 'Imports', 'LinkingTo'))
    travis_time_end
)

# compile base/db
(
    travis_time_start "make_base_db" "building PECAN [base/db]"
    devtools::install_deps('base/db', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile base/settings
(
    travis_time_start "make_base_settings" "building PECAN [base/settings]"
    devtools::install_deps('base/settings', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile base/visualization
(
    travis_time_start "make_base_visualization" "building PECAN [base/visualization]"
    devtools::install_deps('base/visualization', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile base/qaqc
(
    travis_time_start "make_base_qaqc" "building PECAN [base/qaqc]"
    devtools::install_deps('base/qaqc', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile modules/data.atmosphere
(
    travis_time_start "make_modules_data.atmosphere" "building PECAN [modules/data.atmosphere]"
    devtools::install_deps('modules/data.atmosphere', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile modules/data.land
(
    travis_time_start "make_modules_data.land" "building PECAN [modules/data.land]"
    devtools::install_deps('modules/data.land', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile modules/priors
(
    travis_time_start "make_modules_priors" "building PECAN [modules/priors]"
    devtools::install_deps('modules/priors', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile modules/uncertainty
(
    travis_time_start "make_modules_uncertainty" "building PECAN [modules/uncertainty]"
    devtools::install_deps('modules/uncertainty', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile base/workflow
(
    travis_time_start "make_base_workflow" "building PECAN [base/workflow]"
    devtools::install_deps('base/workflow', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile modules/allometry
(
    travis_time_start "make_modules_allometry" "building PECAN [modules/allometry]"
    devtools::install_deps('modules/allometry', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile modules/meta.analysis
(
    travis_time_start "make_modules_meta.analysis" "building PECAN [modules/meta.analysis]"
    devtools::install_deps('modules/meta.analysis', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile modules/assim.batch
(
    travis_time_start "make_modules_assim.batch" "building PECAN [modules/assim.batch]"
    devtools::install_deps('modules/assim.batch', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile modules/assim.sequential
(
    travis_time_start "make_modules_assim.sequential" "building PECAN [modules/assim.sequential]"
    devtools::install_deps('modules/assim.sequential', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile modules/benchmark
(
    travis_time_start "make_modules_benchmark" "building PECAN [modules/benchmark]"
    devtools::install_deps('modules/benchmark', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile modules/data.hydrology
(
    travis_time_start "make_modules_data.hydrology" "building PECAN [modules/data.hydrology]"
    devtools::install_deps('modules/data.hydrology', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile modules/data.remote
(
    travis_time_start "make_modules_data.remote" "building PECAN [modules/data.remote]"
    devtools::install_deps('modules/data.remote', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

# compile modules/photosynthesis
(
    travis_time_start "make_modules_photosynthesis" "building PECAN [modules/photosynthesis]"
    devtools::install_deps('modules/photosynthesis', Ncpus = 1, dependencies = TRUE)
    travis_time_end
)

