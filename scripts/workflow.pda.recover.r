library(PEcAn.all)

# Insert settings file path here. Use the same one supplied to the original failed workflow
settings.file = "/fs/data2/rykelly/PDA/Tests/nr1.test.07/pecan.pda1000001559.xml"

# Read in settings
settings <- read.settings(settings.file)

# Clean and wrap up failed PDA MCMC
settings$assim.batch <- pda.mcmc.recover(settings)

# If desired, now proceed to complete the run
status.start("PDA")
settings <- assim.batch(settings)
status.end()
