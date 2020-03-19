Adding the following to the workflow.R or to your .Rprofile will enable
printing of a stacktrace in case something goes wrong. This will help
with the development of PEcAn.

```R
# ----------------------------------------------------------------------
# debug options
# ----------------------------------------------------------------------
options(warn = 1, keep.source = TRUE, error =
          quote({
            # Need all these try() calls to make sure all wrapup steps are
            # executed, even if some steps generate errors of their own

            try(status.end("ERROR"))

            try(db.print.connections())
            try(cat("Environment:\n", file=stderr()));

            # TODO: setup option for dumping to a file (?)
            # Set `to.file` argument to write this to a file for post-mortem debugging
            try(dump.frames());  # writes to last.dump

            #
            # Debugging in R
            #   http://www.stats.uwo.ca/faculty/murdoch/software/debuggingR/index.shtml
            #
            # Post-mortem debugging
            #   http://www.stats.uwo.ca/faculty/murdoch/software/debuggingR/pmd.shtml
            #
            # Relation functions:
            #   dump.frames
            #   recover
            # >>limitedLabels  (formatting of the dump with source/line numbers)
            #   sys.frame (and associated)
            #   traceback
            #   geterrmessage
            #
            # Output based on the debugger function definition.

            try({
                n <- length(last.dump)
                calls <- names(last.dump)
                cat(paste("  ", 1L:n, ": ", calls, sep = ""), sep = "\n", file=stderr())
                cat("\n", file=stderr())})
            if (!interactive()) {
              q()
            }
          }))
```
