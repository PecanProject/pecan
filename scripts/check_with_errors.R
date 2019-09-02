
arg <- commandArgs(trailingOnly = TRUE)
pkg <- arg[1]

# Workaround for devtools/#1914:
# check() sets its own values for `_R_CHECK_*` environment variables, without
# checking whether any are already set. It winds up string-concatenating new
# onto old (e.g. "FALSE TRUE") instead of either respecting or overriding them.
# (Fixed in devtools 2.0.1.9000; remove these lines after next CRAN release)
Sys.unsetenv(
    c('_R_CHECK_CRAN_INCOMING_',
    '_R_CHECK_CRAN_INCOMING_REMOTE_',
    '_R_CHECK_FORCE_SUGGESTS_'))

log_level <- Sys.getenv('LOGLEVEL', unset = NA)
die_level <- Sys.getenv('DIELEVEL', unset = NA)
redocument <- as.logical(Sys.getenv('REBUILD_DOCS', unset = NA))
runtests <- as.logical(Sys.getenv('RUN_TESTS', unset = FALSE))

# message('log_level = ', log_level)
# message('die_level = ', die_level)

# should test se run
if (as.logical(Sys.getenv('RUN_TESTS', unset = FALSE))) {
    args <- c('--no-tests', '--timings')
} else {
    args <- c('--timings') 
}

valid_log_levels <- c('warn', 'all')
if (!is.na(log_level) && !log_level %in% valid_log_levels) {
    stop('Invalid log_level "', log_level, '". Select one of: ', 
         paste(valid_log_levels, collapse = ', '))
}

if (!is.na(die_level) && !die_level == 'warn') {
    stop('Invalid die_level "', die_level, 
         '". Use either "warn" for warnings or leave blank for error.')
}

log_warn <- !is.na(log_level) && log_level %in% c('warn', 'all')
die_warn <- !is.na(die_level) && die_level == 'warn'

log_notes <- !is.na(log_level) && log_level == 'all'

chk <- devtools::check(pkg, args = args, quiet = TRUE, error_on = "never", document = redocument)

errors <- chk[['errors']]
n_errors <- length(errors)

if (n_errors > 0) {
    cat(errors, '\n')
    stop(n_errors, ' errors found in ', pkg, '. See above for details')
}

warns <- chk[['warnings']]
n_warns <- length(warns)
message(n_warns, ' warnings found in ', pkg, '.')

if ((log_warn|die_warn) && n_warns > 0) {
    cat(warns, '\n')
    if (die_warn && n_warns > 0) {
        stop('Killing process because ', n_warns, ' warnings found in ', pkg, '.')
    }
}

notes <- chk[['notes']]
n_notes <- length(notes)
message(n_notes, ' notes found in ', pkg, '.')

if (log_notes && n_notes > 0) {
    cat(notes, '\n')
}



# PEcAn has a lot of legacy code that issues check warnings,
# such that it's not yet practical to break the build on every warning.
# Cleaning this up is a long-term goal, but will take time.
# Meanwhile, we compare against a cached historic check output to enforce that
# no *new* warnings are added. As historic warnings are removed, we will update
# the cached results to ensure they stay gone.

# Rcmdcheck identifies unique top-level warnings (e.g. "checking Rd cross-references ... WARNING"),
# but does not compare entire contents of warning (e.g. bad cross-references in one file counted same as in three different files)
# We want to get fussier and only allow existing *instances* of warnings, so let's parse a little more finely
msg_lines <- function(msg){
    msg <- strsplit(
        gsub("\n  ", " ", msg, fixed = TRUE), #leading double-space indicates line wrap
        split = "\n",
        fixed = TRUE)
    msg <- purrr::map(msg, ~.[. != ""])
    purrr::flatten_chr(purrr::map(msg, ~paste(.[[1]], .[-1], sep=": ")))
}

old_file <- file.path(pkg, "tests", "Rcheck_reference.log")
if (! file.exists(old_file)) {
   # no reference output available, nothing else to do
   quit("no")
}

old <- rcmdcheck::parse_check(old_file)
cmp <- rcmdcheck::compare_checks(old, chk)


if (cmp$status != "+") {
    print(cmp)
    stop("R check of ", pkg, " reports new problems. Please fix them and resubmit.")
} else {
    # No new messages, but need to check details of pre-existing ones
    warn_cmp <- dplyr::filter(cmp$cmp, type == "warning") # stopped earlier for errors, notes let slide for now
    reseen_msgs <- msg_lines(dplyr::filter(warn_cmp, which=="new")$output)
    prev_msgs <- msg_lines(dplyr::filter(warn_cmp, which=="old")$output)

    lines_changed <- setdiff(reseen_msgs, prev_msgs)
    if (length(lines_changed) > 0) {
        print("Package check returned new warnings:")
        print(lines_changed)
        print("Please fix these and resubmit.")
    }
}

# If want to update saved result to use current check for future comparison:
# cat(chk$stdout, file = old_file)
