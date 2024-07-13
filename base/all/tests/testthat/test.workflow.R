# TODO This is an integration test (#1125)

#context("tests of overall workflow")

#settings.file <- system.file("inst/extdata/test.settings.xml", package = "PEcAn.utils")
#settings <- read.settings(settings.file)

# settings$pfts <- get.trait.data(settings$pfts, settings$model$type, settings$database$dbfiles, settings$database$bety, settings$meta.analysis$update)
# run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$meta.analysis$random.effects$on, settings$meta.analysis$threshold, settings$database$dbfiles, settings$database$bety)
# run.write.configs("ED2")
# clear.scratch(settings)
# start_model_runs("ED2")
# get.results(settings)
