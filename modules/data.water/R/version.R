# Set at package install time, used by pecan.all::pecan_version()
# to identify development versions of packages
.build_hash <- Sys.getenv("PECAN_GIT_REV", "unknown")
