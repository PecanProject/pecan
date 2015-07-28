library(PEcAn.all)

# Insert settings file path here. Use the same one supplied to the original failed workflow
settings.file = ""

# Read in settings
settings <- read.settings(settings.file)

# Clean and wrap up failed PDA MCMC
settings$assim.batch <- pda.mcmc.recover(settings)

# If desired, now proceed to complete the run
status.start("PDA")
settings <- assim.batch(settings)
status.end()
