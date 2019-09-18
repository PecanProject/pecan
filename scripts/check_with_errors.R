
arg <- commandArgs(trailingOnly = TRUE)
pkg <- arg[1]

log_level <- Sys.getenv('LOGLEVEL', unset = NA)
die_level <- Sys.getenv('DIELEVEL', unset = NA)
redocument <- as.logical(Sys.getenv('REBUILD_DOCS', unset = NA))
runtests <- as.logical(Sys.getenv('RUN_TESTS', unset = TRUE))

# message('log_level = ', log_level)
# message('die_level = ', die_level)

# should test se run
if (!runtests) {
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
#
# To compare checks, we take a two-level approach:
# First by comparing results with rcmdcheck::compare_checks to find any new
# top-level warnings (e.g. "checking Rd cross-references ... WARNING"),
# then if those are OK we get fussier and check for new *instances* of existing
# warnings (e.g. new check increases from 2 bad Rd cross-references to 3).

msg_lines <- function(msg){
    msg <- strsplit(
        gsub("\n  ", " ", msg, fixed = TRUE), #leading double-space indicates line wrap
        split = "\n",
        fixed = TRUE)
    msg <- lapply(msg, function(x)x[x != ""])
    unlist(lapply(msg, function(x)paste(x[[1]], x[-1], sep=": ")))
}

old_file <- file.path(pkg, "tests", "Rcheck_reference.log")

# To update reference files after fixing an old warning:
# * Run check_with_errors.R to be sure the check is currently passing
# * Delete the file you want to update
# * Uncomment this section, run check_with_errors.R, recomment
# * Commit updated file
# if (! file.exists(old_file)) {
#    cat("No reference check file found. Saving current results as the new standard\n")
#    cat(chk$stdout, file = old_file)
#    quit("no")
# }

old <- rcmdcheck::parse_check(old_file)
cmp <- rcmdcheck::compare_checks(old, chk)


if (cmp$status != "+") {
    # rcmdcheck found new messages, so check has failed
    print(cmp)
    cat("R check of", pkg, "reports the following new problems.",
        "Please fix these and resubmit:\n")
    cat(cmp$cmp$output[cmp$cmp$change == 1], sep = "\n")
    stop("Please fix these and resubmit.")
} else {
    # No new messages, but need to check details of pre-existing ones
    # We stopped earlier for errors, so all entries here are WARNING or NOTE
    cur_msgs <- msg_lines(cmp$cmp$output[cmp$cmp$which == "new"])
    prev_msgs <- msg_lines(cmp$cmp$output[cmp$cmp$which == "old"])

    # avoids false positives from tempdir changes
    cur_msgs <- gsub(chk$checkdir, "...", cur_msgs)
    prev_msgs <- gsub(old$checkdir, "...", prev_msgs)

    # Different R versions seem to differ on straight vs fancy quotes
    cur_msgs <- gsub("[‘’]", "'", cur_msgs)
    prev_msgs <- gsub("[‘’]", "'", prev_msgs)

    # Compression warnings report slightly different sizes on different R versions
    # If the only difference is in the numbers, don't complain
    cmprs_msg <- grepl("significantly better compression", cur_msgs)
    if(any(cmprs_msg)){
        prev_cmprs_msg <- grepl("significantly better compression", prev_msgs)
        cur_cmprs_nodigit <- gsub("[0-9]", "", cur_msgs[cmprs_msg])
        prev_cmprs_nodigit <- gsub("[0-9]", "", prev_msgs[prev_cmprs_msg])
        if(all(cur_cmprs_nodigit %in% prev_cmprs_nodigit)){
            cur_msgs <- cur_msgs[!cmprs_msg]
        }
    }

    lines_changed <- setdiff(cur_msgs, prev_msgs)
    if (length(lines_changed) > 0) {
        cat("R check of", pkg, "returned new problems:\n")
        cat(lines_changed, sep = "\n")
        stop("Please fix these and resubmit.")
    }
}
