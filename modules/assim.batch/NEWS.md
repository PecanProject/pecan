# PEcAn.assim.batch 1.8.0.9000



# PEcAn.assim.batch 1.8.0

## Breaking changes
* In functions `gpeval` and `mcmc.GP`, parameter `splinefcns` has been renamed to `splinefuns` to match the spelling in `minimize.GP`, thereby also fixing several cases where the wrong name was passed between functions.

## Added
* Functions `bounded`, `calculate.prior`, `ddist`, `get_ss`, `get_y`, `gepeval`, `is.accepted`, `mcmc.GP`, `minimize.GP`, all used for sampling from Gaussian processes, have been moved here from package `PEcAn.emulator` to correct a circular dependency.


# PEcAn.assim.batch 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the PEcAn packages; please see https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
